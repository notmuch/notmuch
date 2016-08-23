package main

// stdlib imports
import "os"
import "path"
import "log"
import "fmt"
import "regexp"
import "strings"
import "sort"

// 3rd-party imports
import "notmuch"
import "github.com/msbranco/goconfig"

type mail_addr_freq struct {
	addr  string
	count [3]uint
}

type frequencies map[string]uint

/* Used to sort the email addresses from most to least used */
func sort_by_freq(m1, m2 *mail_addr_freq) int {
	if m1.count[0] == m2.count[0] &&
		m1.count[1] == m2.count[1] &&
		m1.count[2] == m2.count[2] {
		return 0
	}

	if m1.count[0] > m2.count[0] ||
		m1.count[0] == m2.count[0] &&
			m1.count[1] > m2.count[1] ||
		m1.count[0] == m2.count[0] &&
			m1.count[1] == m2.count[1] &&
			m1.count[2] > m2.count[2] {
		return -1
	}

	return 1
}

type maddresses []*mail_addr_freq

func (self *maddresses) Len() int {
	return len(*self)
}

func (self *maddresses) Less(i, j int) bool {
	m1 := (*self)[i]
	m2 := (*self)[j]
	v := sort_by_freq(m1, m2)
	if v <= 0 {
		return true
	}
	return false
}

func (self *maddresses) Swap(i, j int) {
	(*self)[i], (*self)[j] = (*self)[j], (*self)[i]
}

// find most frequent real name for each mail address
func frequent_fullname(freqs frequencies) string {
	var maxfreq uint = 0
	fullname := ""
	freqs_sz := len(freqs)

	for mail, freq := range freqs {
		if (freq > maxfreq && mail != "") || freqs_sz == 1 {
			// only use the entry if it has a real name
			// or if this is the only entry
			maxfreq = freq
			fullname = mail
		}
	}
	return fullname
}

func addresses_by_frequency(msgs *notmuch.Messages, name string, pass uint, addr_to_realname *map[string]*frequencies) *frequencies {

	freqs := make(frequencies)

	pattern := `\s*(("(\.|[^"])*"|[^,])*<?(?mail\b\w+([-+.]\w+)*\@\w+[-\.\w]*\.([-\.\w]+)*\w\b)>?)`
	// pattern := "\\s*((\\\"(\\\\.|[^\\\\\"])*\\\"|[^,])*" +
	// 	"<?(?P<mail>\\b\\w+([-+.]\\w+)*\\@\\w+[-\\.\\w]*\\.([-\\.\\w]+)*\\w\\b)>?)"
	pattern = `.*` + strings.ToLower(name) + `.*`
	var re *regexp.Regexp = nil
	var err error = nil
	if re, err = regexp.Compile(pattern); err != nil {
		log.Printf("error: %v\n", err)
		return &freqs
	}

	headers := []string{"from"}
	if pass == 1 {
		headers = append(headers, "to", "cc", "bcc")
	}

	for ; msgs.Valid(); msgs.MoveToNext() {
		msg := msgs.Get()
		//println("==> msg [", msg.GetMessageId(), "]")
		for _, header := range headers {
			froms := strings.ToLower(msg.GetHeader(header))
			//println("  froms: ["+froms+"]")
			for _, from := range strings.Split(froms, ",") {
				from = strings.Trim(from, " ")
				match := re.FindString(from)
				//println("  -> match: ["+match+"]")
				occ, ok := freqs[match]
				if !ok {
					freqs[match] = 0
					occ = 0
				}
				freqs[match] = occ + 1
			}
		}
	}
	return &freqs
}

func search_address_passes(queries [3]*notmuch.Query, name string) []string {
	var val []string
	addr_freq := make(map[string]*mail_addr_freq)
	addr_to_realname := make(map[string]*frequencies)

	var pass uint = 0 // 0-based
	for _, query := range queries {
		if query == nil {
			//println("**warning: idx [",idx,"] contains a nil query")
			continue
		}
		msgs := query.SearchMessages()
		ht := addresses_by_frequency(msgs, name, pass, &addr_to_realname)
		for addr, count := range *ht {
			freq, ok := addr_freq[addr]
			if !ok {
				freq = &mail_addr_freq{addr: addr, count: [3]uint{0, 0, 0}}
			}
			freq.count[pass] = count
			addr_freq[addr] = freq
		}
		msgs.Destroy()
		pass += 1
	}

	addrs := make(maddresses, len(addr_freq))
	{
		iaddr := 0
		for _, freq := range addr_freq {
			addrs[iaddr] = freq
			iaddr += 1
		}
	}
	sort.Sort(&addrs)

	for _, addr := range addrs {
		freqs, ok := addr_to_realname[addr.addr]
		if ok {
			val = append(val, frequent_fullname(*freqs))
		} else {
			val = append(val, addr.addr)
		}
	}
	//println("val:",val)
	return val
}

type address_matcher struct {
	// the notmuch database
	db *notmuch.Database
	// full path of the notmuch database
	user_db_path string
	// user primary email
	user_primary_email string
	// user tag to mark from addresses as in the address book
	user_addrbook_tag string
}

func new_address_matcher() *address_matcher {
	// honor NOTMUCH_CONFIG
	home := os.Getenv("NOTMUCH_CONFIG")
	if home == "" {
		home = os.Getenv("HOME")
	}

	cfg, err := goconfig.ReadConfigFile(path.Join(home, ".notmuch-config"))
	if err != nil {
		log.Fatalf("error loading config file:", err)
	}

	db_path, _ := cfg.GetString("database", "path")
	primary_email, _ := cfg.GetString("user", "primary_email")
	addrbook_tag, err := cfg.GetString("user", "addrbook_tag")
	if err != nil {
		addrbook_tag = "addressbook"
	}

	self := &address_matcher{db: nil,
		user_db_path:       db_path,
		user_primary_email: primary_email,
		user_addrbook_tag:  addrbook_tag}
	return self
}

func (self *address_matcher) run(name string) {
	queries := [3]*notmuch.Query{}

	// open the database
	if db, status := notmuch.OpenDatabase(self.user_db_path,
		notmuch.DATABASE_MODE_READ_ONLY); status == notmuch.STATUS_SUCCESS {
		self.db = db
	} else {
		log.Fatalf("Failed to open the database: %v\n", status)
	}

	// pass 1: look at all from: addresses with the address book tag
	query := "tag:" + self.user_addrbook_tag
	if name != "" {
		query = query + " and from:" + name + "*"
	}
	queries[0] = self.db.CreateQuery(query)

	// pass 2: look at all to: addresses sent from our primary mail
	query = ""
	if name != "" {
		query = "to:" + name + "*"
	}
	if self.user_primary_email != "" {
		query = query + " from:" + self.user_primary_email
	}
	queries[1] = self.db.CreateQuery(query)

	// if that leads only to a few hits, we check every from too
	if queries[0].CountMessages()+queries[1].CountMessages() < 10 {
		query = ""
		if name != "" {
			query = "from:" + name + "*"
		}
		queries[2] = self.db.CreateQuery(query)
	}

	// actually retrieve and sort addresses
	results := search_address_passes(queries, name)
	for _, v := range results {
		if v != "" && v != "\n" {
			fmt.Println(v)
		}
	}
	return
}

func main() {
	//fmt.Println("args:",os.Args)
	app := new_address_matcher()
	name := ""
	if len(os.Args) > 1 {
		name = os.Args[1]
	}
	app.run(name)
}
