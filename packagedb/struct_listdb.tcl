# This syntax definition is for the struct::list package
# package require struct::list 1.8.1

set ::syntax(struct::list::list) {s x*}
set ::subcmd(struct::list::list) {
    assign
    dbJoin
    dbJoinKeyed
    delete
    equal
    filter
    filterfor
    firstperm
    flatten
    fold
    foreachperm
    iota
    lcsInvert
    lcsInvert2
    lcsInvertMerge
    lcsInvertMerge2
    longestCommonSubsequence
    longestCommonSubsequence2
    map
    mapfor
    nextperm
    permutations
    repeat
    repeatn
    reverse
    shift
    shuffle
    split
    swap
}

set ::syntax(struct::list::list\ assign) {r 2}
set ::syntax(struct::list::list\ dbJoin) {r 0}
set ::syntax(struct::list::list\ dbJoinKeyed) {r 0}
set ::syntax(struct::list::list\ delete) 2
set ::syntax(struct::list::list\ equal) 2
set ::syntax(struct::list::list\ filter) 2
set ::syntax(struct::list::list\ filterfor) 3
set ::syntax(struct::list::list\ firstperm) 1
set ::syntax(struct::list::list\ flatten) {r 0}
set ::syntax(struct::list::list\ fold) 3
set ::syntax(struct::list::list\ foreachperm) 3
set ::syntax(struct::list::list\ iota) 1
set ::syntax(struct::list::list\ lcsInvert) 3
set ::syntax(struct::list::list\ lcsInvert2) 4
set ::syntax(struct::list::list\ lcsInvertMerge) 3
set ::syntax(struct::list::list\ lcsInvertMerge2) 4
set ::syntax(struct::list::list\ longestCommonSubsequence) {r 2 3}
set ::syntax(struct::list::list\ longestCommonSubsequence2) {r 2 3}
set ::syntax(struct::list::list\ map) 2
set ::syntax(struct::list::list\ mapfor) 3
set ::syntax(struct::list::list\ nextperm) 1
set ::syntax(struct::list::list\ permutations) 1
set ::syntax(struct::list::list\ repeat) {r 2}
set ::syntax(struct::list::list\ repeatn) {r 1}
set ::syntax(struct::list::list\ reverse) 1
set ::syntax(struct::list::list\ shift) 1
set ::syntax(struct::list::list\ shuffle) 1
set ::syntax(struct::list::list\ split) {r 2}
set ::syntax(struct::list::list\ swap) 3

set ::syntax(struct::list) $::syntax(struct::list::list)
set ::subcmd(struct::list) $::subcmd(struct::list::list)

foreach item [array names ::syntax "struct::list::list *"] {
    # struct::list copy
    set item2 [string map {{::list } { }} $item]
    set ::syntax($item2) $::syntax($item)

    # Make the internal functions visible too
    set item2 [string map {{list } {L}} $item]
    set ::syntax($item2) $::syntax($item)
}

lappend ::knownPackages struct::list
