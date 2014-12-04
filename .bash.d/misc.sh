random-sin() {
    xidel -q http://www.fakenamegenerator.com/social-insurance-number.php --css="#details .content p" | awk 'NR == 2' | awk '{$1=$2=""; print $0}' | tr -d ' '
}
