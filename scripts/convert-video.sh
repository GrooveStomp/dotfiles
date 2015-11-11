function convert-video() {
    local num_args="$#"
    if [[ "$num_args" -ne 3 ]] || [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]]; then
        echo "Usage: ${FUNCNAME[0]} input_video subtitle_.srt_file output_.mp4_file"
        echo "  eg.: ${FUNCNAME[0]} avengers.mkv subtitles/avengers.srt avengers.mp4"
        echo "       -h|--help: This output"
        return
    fi
    ffmpeg -i $1 -vf subtitles=$2 -q:a 0 -q:v 0 -acodec ac3 -strict experimental $3
}
