random-sin() {
    xidel -q http://www.fakenamegenerator.com/social-insurance-number.php --css="#details .content p" | awk 'NR == 2' | awk '{$1=$2=""; print $0}' | tr -d ' '
}

vmware-recover-keyboard() {
    echo "WARNING! Specific to System76 Galago UltraPro with Kinesis Advantage keyboard."
    setxkbmap -model pc105 -layout us -option -option ',winkeys' -option 'caps:ctrl-modifier'
    # The following command shows current settings
    # setxkbmap -query
}
