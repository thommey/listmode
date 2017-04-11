# listmode.tcl
# Provides Eggdrop with a way to handle arbitrary list modes
###################
# (c) 2017 Thomas Sader (thommey)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# 1. Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
###################
# Settings
###################

# The modes this should manage/understand
# key/value list of <command> <modechar>
set ::listmode_commands {"quiet" "q"}

# Filename for storage (e.g. bot123.listmode)
set ::listmode_file "LamestBot.listmode"

# Timezone for formatting expiry times
set ::listmode_tz "Europe/Berlin"

# Default reason if not specified
set ::listmode_noreason "No reason."

###################
# End of Settings
###################

package require Tcl 8.5
package require eggdrop 1.6.21

###################
# Storage/Initialization
###################

proc listmode_init {} {
	listmode_load
	listmode_bind
	listmode_help
	if {$::server ne "" && (![info exists ::modeconfig] || $::modeconfig eq "" || ![info exists ::chantypes] || $::chantypes eq "")} {
		putserv "VERSION"
	}
}

proc listmode_help {} {
	global help-path listmode_commands
	if {[catch {open [file join ${help-path} listmode.help] w} fs]} {
		putlog "Listmode: WARNING: Could not create help-file in ${help-path}: $fs"
		return
	}
	dict for {command modechar} $listmode_commands {
		puts $fs "%{help=+$command}%{+m|m}"
		puts $fs "###  %b+$command%b \[channel] <hostmask> \[%%<XdXhXm>] \[reason]"
		puts $fs "Adds a mask to the list of +$modechar masks stored on the bot, with optional reason and"
		puts $fs "ban time. This ban is stored with your handle as the creator, and will be"
		puts $fs "in effect for every channel if no channel is specified. Prefixing a comment"
		puts $fs "See also: ${command}s, -$command"
		puts $fs "%{help=-$command}%{+m|m}"
		puts $fs "###  %b-$command%b <mask id>"
		puts $fs "Delete a mask from the list of +$modechar masks stored on the bot."
		puts $fs "The ID is from %b${command}s%b."
		puts $fs "See also: ${command}s, +$command"
		puts $fs "%{help=${command}s}%{+m|m}"
		puts $fs "###  %b${command}s%b \[channel]"
		puts $fs "Lists all masks added to the list of +$modechar masks stored on the bot."
		puts $fs "If a channel is specified, it lists both the global and channel masks."
		puts $fs "See also: +$command, -$command"
	}
	close $fs
	loadhelp listmode.help
}

proc listmode_bind {} {
	global listmode_commands listmode_modechars
	set listmode_modechars ""
	set commands ""
	# Expand listmode commands into individual binds
	dict for {command modechar} $listmode_commands {
		if {[string length $modechar] != 1} {
			putlog "Lastmodes: ERROR: '$modechar' is not a single character."
			return
		}
		dict set commands "+$command" [list listmode_dcc_pls $modechar]
		dict set commands "-$command" [list listmode_dcc_mns]
		dict set commands "$command" [list listmode_dcc_show $modechar]
		dict set commands "${command}s" [list listmode_dcc_show $modechar]
		append listmode_modechars $modechar
	}
	foreach bind [binds listmode_*] {
		lassign $bind type flags name hits proc
		if {[string match listmode_* $proc] && ![dict exists $commands $name]} {
			unbind $type $flags $name $proc
		}
	}
	dict for {name proc} $commands {
		bind dcc m|m "$name" $proc
	}
	bind raw - MODE listmode_parsemode
	bind raw - 005 listmode_parse005
	bind join - * listmode_join
	bind time - * listmode_checkexpiry
}

proc listmode_load {} {
	global listmode_file listmode_data
	if {![file exists $listmode_file] || ![file readable $listmode_file]} {
		close [open $listmode_file w]
	}
	set fs [open $listmode_file r]
	set listmode_data [read -nonewline $fs]
	close $fs
	listmode_checkexpiry
}

proc listmode_save {} {
	global listmode_file listmode_data
	set listmode_data [dict filter $listmode_data script {id info} { expr {[dict get $info chan] eq "global" || [validchan [dict get $info chan]]} }]
	set fs [open $listmode_file.bak w]
	puts -nonewline $fs $listmode_data
	close $fs
	file copy -force $listmode_file.bak $listmode_file
}

###################
# DCC Commands
###################

proc listmode_dcc_pls {pre hand idx text} {
	global listmode_data
	set text [join [list $pre {*}[split $text]]]
	lassign [listmode_parseargs $hand $text] mc chan arg duration reason
	if {$arg eq ""} {
		putdcc $idx "Syntax: $::lastbind [join [lrange [split "<modechar> \[#channel\] <mask> \[%duration\] \[reason\]"] [llength $pre] end]], duration example: %12h5m"
		return
	}
	if {[catch {listmode_add $chan $mc $arg $hand $duration $reason} err]} {
		putdcc $idx "Failed to add entry: $err"
	} else {
		putdcc $idx "Added: [listmode_formatentry $err]"
	}
	return 1
}

proc listmode_dcc_mns {hand idx text} {
	global listmode_data listmode_modechars
	if {$text eq "" || ![string is digit -strict $text]} {
		putdcc $idx "Syntax: $::lastbind <id>"
		return
	}
	set entry [listmode_getbyid $text]
	if {$entry eq ""} {
		putdcc $idx "Unknown mask id."
		return
	}
	set flags [expr {[dict get $entry chan] eq "global" ? "m" : [list m|m $chan]}]
	if {![matchattr $hand {*}$flags]} {
		putdcc $idx "No access."
		return
	}
	set entry [listmode_del $text]
	putdcc $idx "Deleted: [listmode_formatentry $entry]"
	return 1
}

proc listmode_dcc_show {pre hand idx text} {
	set text [join [list $pre {*}[split $text]]]
	set chans global
	lassign [split $text] mc text
	if {$mc ni [split $::listmode_modechars ""]} {
		putdcc $idx "Syntax: $::lastbind <modechar> \[#chan\]"
		return
	}
	if {$text ne ""} {
		if {[string index $text 0] ni [split $::chantypes ""] || ![validchan $text]} {
			putdcc $idx "Invalid channel."
			return
		}
		lappend chans $text
	}
	foreach chan $chans {
		putdcc $idx "--- $chan masks ---"
		set entries [listmode_getdata $chan $mc]
		if {![dict size $entries]} {
			putdcc $idx "  (none)"
		} else {
			dict for {id entry} $entries {
				putdcc $idx "  [listmode_formatentry [dict create $id $entry]]"
			}
		}
	}
	return 1
}

###################
# General Functions
###################

proc listmode_formatentry {entry} {
	lassign $entry id info
	dict with info {
		return [format {[%5s] %s created by: %s, expires: %s, reason: %s} $id $mask $creator [expr {$expiry ? [clock format $expiry -timezone $::listmode_tz] : "never"}] $reason]
	}
}

proc listmode_parseword {textVar destVar {rule 1}} {
	upvar 1 $textVar text
	upvar 1 $destVar word
	set word [lindex $text 0]
	if $rule {
		set text [lrange $text 1 end]
	} else {
		set word ""
	}
}

proc listmode_parseargs {hand text} {
	global listmode_modechars
	set text [split $text]
	set text [lassign $text mc]
	if {$mc ni $::listmode_modechars} {
		error "Invalid modechar '$mc', supporting '$listmode_modechars'"
	}
	listmode_parseword text chan {[string index $word 0] in [split $::chantypes ""]}
	if {$chan ne "" && ![validchan $chan]} {
		error "Invalid channel $chan"
	}
	set flags [expr {$chan ne "" ? "m" : [list m|m $chan]}]
	if {![matchattr $hand {*}$flags]} {
		error "No access."
	}
	listmode_parseword text arg
	if {$chan eq ""} {
		listmode_parseword text chan {[string index $word 0] in [split $::chantypes ""]}
		if {$chan ne "" && ![validchan $chan]} {
			error "Invalid channel $chan"
		}
	}
	listmode_parseword text duration {[string index $word 0] eq "%"}
	if {$duration ne "" && $duration != 0} {
		set duration [listmode_parseduration [string range $duration 1 end]]
	}
	return [list $mc $chan $arg $duration [join $text]]
}

proc listmode_getdata {chan mc} {
	global listmode_data
	listmode_getby $listmode_data {[string equal -nocase [dict get $entry chan] [lindex $args 0]] && [dict get $entry mc] eq [lindex $args 1]} $chan $mc
}

proc listmode_getby {data condition args} {
	dict filter $data script {id entry} [list expr $condition]
}

proc listmode_getbyid {id} {
	global listmode_data
	if {![dict exists $listmode_data $id]} {
		return ""
	}
	return [dict get $listmode_data $id]
}

proc listmode_getbymask {data mask} {
	listmode_getby $data {[dict get $entry mask] eq [lindex $args 0]} $mask
}

proc listmode_getbychan {data chan} {
	listmode_getby $data {[string equal -nocase [dict get $entry chan] [lindex $args 0]]} $chan
}

proc listmode_setdata {id chan mc mask creator expiry reason} {
	global listmode_data
	set this [dict create chan $chan mc $mc mask $mask creator $creator expiry $expiry reason $reason]
	dict set listmode_data $id $this
	listmode_save
	return [dict create $id $this]
}

proc listmode_unset {id} {
	global listmode_data
	set entry [dict get $listmode_data $id]
	dict unset listmode_data $id
	listmode_save
	return [dict create $id $entry]
}

proc listmode_del {id} {
	set entry [dict get [listmode_unset $id] $id]
	set chans [expr {[dict get $entry chan] eq "global" ? [channels] : [list [dict get $entry chan]]}]
	foreach chan $chans {
		set chandata [listmode_getdata $chan [dict get $entry mc]]
		if {[botonchan $chan] && [botisop $chan] && ![dict size [listmode_getbymask $chandata [dict get $entry mask]]]} {
			putquick "MODE $chan -[dict get $entry mc] [dict get $entry mask]"
		}
	}
	listmode_save
	return [dict create $id $entry]
}

proc listmode_add {chan mc mask hand duration reason} {
	if {$chan eq ""} {
		set chan "global"
	}
	set expiry [expr {$duration eq "" || $duration == 0 ? 0 : [clock add [clock seconds] $duration seconds]}]
	if {$reason eq ""} {
		set reason $::listmode_noreason
	}
	set data [listmode_getdata $chan $mc]
	set existing [lrange [listmode_getbymask $data $mask] 0 1]
	if {[dict size $existing]} {
		error "Mask already exists: [listmode_formatentry $existing]"
	}
	set id [listmode_newid]
	set chans [expr {$chan eq "global" ? [channels] : [list $chan]}]
	foreach ch $chans {
		if {[botonchan $ch] && [botisop $ch]} {
			putquick "MODE $ch +$mc $mask"
		}
	}
	return [listmode_setdata $id $chan $mc $mask $hand $expiry $reason]
}

proc listmode_newid {} {
	global listmode_data
	set existing [dict keys $listmode_data]
	for {set id 1} {$id < 32768 && $id in $existing} {incr id} {}	
	if {$id == 32768} {
		error "Listmode: ERROR: No free ID found!"
	}
	return $id
}

# Convert a human readable duration into seconds
# e.g. "1h5m" or "1 hour and 5 minutes"
proc listmode_parseduration {string} {
	# replace some characters to unify parsing
	set string [string map {" and " "" " " "" "," ""} $string]
	set result 0
	foreach {full amount unit} [regexp -all -inline -- {([+-]?(?:[0-9]+|[0-9]*\.[0-9]+))([^0-9.+-]*)} $string] {
		if {$unit eq ""} {
			set unit s
		}
		if {[scan $amount %f amount] != 1} {
			error "Could not parse '$amount' as number."
		}
		set multiplier [listmode_durationmultiplier [string tolower $unit]]
		if {$multiplier eq ""} {
			error "Could not parse '$unit' as time unit."
		}
		set result [expr {$result + $multiplier * $amount}]
	}
	return [expr {round($result)}]
}

# Helper proc for parseduration
proc listmode_durationmultiplier {unit} {
	set mult {years 31536000 weeks 604800 days 86400 hours 3600 minutes 60 seconds 1}
	if {$unit eq ""} {
		return ""
	}
	if {[llength [info commands tcl::prefix]]} {
		set key [tcl::prefix match [dict keys $mult] $unit]
	} else {
		set key [lsearch -inline -glob [dict keys $mult] $unit*]
	}
	dict get $mult $key
}

###################
# IRC events
###################

proc listmode_chanmode {chan plsmns mc mask} {
	# unused
	return 0
}

proc listmode_join {nick host hand chan} {
	set masks [list {*}[listmode_getbychan $::listmode_data $chan] {*}[listmode_getbychan $::listmode_data global]]
	dict for {id entry} $masks {
		if {[string match -nocase [dict get $entry mask] $nick!$host]} {
			putquick "MODE $chan +[dict get $entry mc] [dict get $entry mask]"
		}
	}
}

proc listmode_checkexpiry {args} {
	global listmode_data
	set expired [listmode_getby $listmode_data {[dict get $entry expiry] != 0 && [dict get $entry expiry] < [clock seconds]}]
	dict for {id entry} $expired {
		putlog "Listmode: INFO: Expiring [listmode_formatentry [dict create $id $entry]]"
		listmode_del $id
	}
}

###################
# Arbitrary mode stuff and utilities
###################

# removes first element from the list and returns it
proc listmode_pop {varname} {
	upvar 1 $varname list
	if {![info exists list]} { return "" }
	set elem [lindex $list 0]
	set list [lrange $list 1 end]
	return $elem
}

proc listmode_parse005 {from key text} {
	if {[regexp {CHANMODES=(\S+)} $text -> modes]} {
		set ::modeconfig [split $modes ,]
		lassign $::modeconfig listmodes
		set newmc ""
		foreach mc [split $::listmode_modechars ""] {
			if {$mc ni [split $listmodes ""]} {
				putlog "Listmodes: WARNING: Mode char '$mc' is not supported by this IRCd but the script is set to manage it. Ignoring it."
			} else {
				append newmc $mc
			}
		}
		set ::listmode_modechars $newmc
	}
	if {[regexp {PREFIX=\((\S+)\)} $text -> umodes]} {
		set ::umodeconfig $umodes
	}
	if {[regexp {CHANTYPES=(\S+)} $text -> types]} {
		set ::chantypes $types
	}
	return 0
}

proc listmode_modeparam {pre modechar} {
	if {![info exists ::umodeconfig]} {
		putlog "Listmodes: Could not get usermodeconfig from raw 005!"
		set ::umodeconfig qaohv
	}
	if {![info exists ::modeconfig]} {
		putlog "Listmodes: Could not get modeconfig from raw 005!"
		set ::modeconfig [split beI,kfL,lj,psmntirRcOAQKVCuzNSMTGZ ,]
	}
	set pls [expr {$pre eq "+"}]
	if {[string match *$modechar* $::umodeconfig] || [string match *$modechar* [lindex $::modeconfig 0]] || [string match *$modechar* [lindex $::modeconfig 1]]} {
		return 1
	}
	if {[string match *$modechar* [lindex $::modeconfig 2]]} {
		return $pls
	}
	if {[string match *$modechar* [lindex $::modeconfig 3]]} {
		return 0
	}
	putlog "Listmodes: Unknown mode char '$modechar'!"
	return 0
}

# "thommey!thommey@tclhelp.net MODE #thommey -v+v TCL ^|^"
proc listmode_parsemode {from key text} {
	set text [split $text]
	set chan [string tolower [lindex $text 0]]
	if {![validchan $chan]} { return }
	foreach {mode victim} [listmode_parsemodestr [lindex $text 1] [lrange $text 2 end]] {
		set victim [string tolower $victim]
		lassign [split $mode ""] plsmns mc
		if {$mc in [split $::listmode_modechars ""]} {
			listmode_chanmode $chan [expr {$plsmns eq "+"}] $mc $victim
		}
	}
	return 0
}

# parses a modestring "+v-v" and a list of victims {nick1 nick2} and returns a flat list in the form {modechange victim modechange victim ..}
# static modelist with parameters taken from unrealircd (better do it dynamically on raw 005 ;)
proc listmode_parsemodestr {modestr victims} {
	set result [list]
	set pre "+"
	foreach char [split $modestr ""] {
		if {$char eq "+" || $char eq "-"} {
			set pre $char
		} else {
			set param [expr {[listmode_modeparam $pre $char] ? [listmode_pop victims] : ""}]
			lappend result $pre$char $param
		}
	}
	set result
}

listmode_init
putlog "Listmodes support loaded"
