function convert-video() {
    if [[ "$num_args" -ne 2 ]] || [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
        echo "Usage: ${FUNCNAME[0]} input_video output_.mp4_file"
        echo "  eg.: ${FUNCNAME[0]} avengers.mkv avengers.mp4"
        echo "       -h|--help: This output"
        return
    fi
    ffmpeg -i $1 -vf subtitles=$1 -q:a 0 -q:v 0 -acodec ac3 -strict experimental $2
}
