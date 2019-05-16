for file in benchmark-data/*.txt; do
    if [[ $file == *"_"* ]]; then
        echo "skipping $file"
    else
        echo $file
        ./compress "$(< $file)"
    fi
done
