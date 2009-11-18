%global git 306635c2

%if %($(pkg-config emacs) ; echo $?)
%global emacs_version 23.1
%global emacs_lispdir %{_datadir}/emacs/site-lisp
%global emacs_startdir %{_datadir}/emacs/site-lisp/site-start.d
%else
%global emacs_version %(pkg-config emacs --modversion)
%global emacs_lispdir %(pkg-config emacs --variable sitepkglispdir)
%global emacs_startdir %(pkg-config emacs --variable sitestartdir)
%endif

Name:           notmuch
Version:        0.0
Release:        0.3.%{git}%{?dist}
Summary:        Not much of an email program

Group:          Applications/Internet
License:        GPLv3+
URL:            http://notmuchmail.org/

#
# To create a tarball:
#
# git clone git://notmuchmail.org/git/notmuch
# cd notmuch
# git archive --format=tar --prefix=notmuch/ HEAD | bzip2 > notmuch-`git show-ref --hash=8 HEAD`.tar.bz2
#
Source0:        notmuch-%{git}.tar.bz2
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)

BuildRequires:  xapian-core-devel
BuildRequires:  gmime-devel
BuildRequires:  libtalloc-devel
BuildRequires:  zlib-devel
BuildRequires:  emacs-el
BuildRequires:  emacs-nox

Requires:       emacs(bin) >= %{emacs_version}

%description
* "Not much mail" is what Notmuch thinks about your email
  collection. Even if you receive 12000 messages per month or have on
  the order of millions of messages that you've been saving for
  decades. Regardless, Notmuch will be able to quickly search all of
  it. It's just plain not much mail.

* "Not much mail" is also what you should have in your inbox at any
  time. Notmuch gives you what you need, (tags and fast search), so
  that you can keep your inbox tamed and focus on what really matters
  in your life, (which is surely not email).

* Notmuch is an answer to Sup. Sup is a very good email program
  written by William Morgan (and others) and is the direct inspiration
  for Notmuch. Notmuch began as an effort to rewrite
  performance-critical pieces of Sup in C rather than ruby. From
  there, it grew into a separate project. One significant contribution
  Notmuch makes compared to Sup is the separation of the
  indexer/searcher from the user interface. (Notmuch provides a
  library interface so that its indexing/searching/tagging features
  can be integrated into any email program.)

* Notmuch is not much of an email program. It doesn't receive messages
  (no POP or IMAP suport). It doesn't send messages (no mail composer,
  no network code at all). And for what it does do (email search) that
  work is provided by an external library, Xapian. So if Notmuch
  provides no user interface and Xapian does all the heavy lifting,
  then what's left here? Not much.

Notmuch is still in the early stages of development, but it does
include one user interface, (implemented within emacs), which has at
least two users using it for reading all of their incoming mail. If
you've been looking for a fast, global-search and tag-based email
reader to use within emacs, then Notmuch may be exactly what you've
been looking for.

Otherwise, if you're a developer of an existing email program and
would love a good library interface for fast, global search with
support for arbitrary tags, then Notmuch also may be exactly what
you've been looking for.

%prep
%setup -q -n notmuch

%build
make %{?_smp_mflags} CFLAGS="%{optflags}"
emacs -batch -f batch-byte-compile notmuch.el

%install
rm -rf %{buildroot}
make install DESTDIR=%{buildroot} prefix=%{_prefix}
mkdir -p %{buildroot}%{emacs_startdir}
install -m0644 -p notmuch.el* %{buildroot}%{emacs_startdir}

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root,-)
%doc AUTHORS COPYING COPYING-GPL-3 INSTALL README TODO

%{_sysconfdir}/bash_completion.d/notmuch
%{_bindir}/notmuch
%{_mandir}/man1/notmuch.1*
%{emacs_startdir}/notmuch.el*

%changelog
* Wed Nov 18 2009 Jeffrey C. Ollie <jeff@ocjtech.us> - 0.0-0.3.306635c2
- First version

