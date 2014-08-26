
# we go through a bit of work to get the unmangled names of the
# typeinfo symbols because of
# http://sourceware.org/bugzilla/show_bug.cgi?id=10326

if [ $# -lt 2 ]; then
    echo Usage: $0 header obj1 obj2 obj3
    exit 1;
fi

HEADER=$1
shift

printf '{\nglobal:\n'
nm  $* | awk '$1 ~ "^[0-9a-fA-F][0-9a-fA-F]*$" && $3 ~ "Xapian.*Error" {print $3}' | sort | uniq | \
while read sym; do
    demangled=$(c++filt $sym)
    case $demangled in
	typeinfo*) 
	    printf "\t$sym;\n"
	    ;;
	*)
	    ;;
    esac
done
nm $* | awk '$1 ~ "^[0-9a-fA-F][0-9a-fA-F]*$" && $2 == "T" && $3 ~ "^(getline|getdelim|canonicalize_file_name)$" {print $3 ";"}'
sed  -n 's/^[[:space:]]*\(notmuch_[a-z_]*\)[[:space:]]*(.*/ \1;/p' $HEADER
printf "local: *;\n};\n"
