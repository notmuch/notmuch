NAME = notmuch-mutt

all: $(NAME) $(NAME).1

$(NAME).1: $(NAME)
	pod2man $< > $@

README.html: README
	markdown $< > $@

clean:
	rm -f notmuch-mutt.1 README.html
