#!/usr/bin/env sh
ARG_QQ =
ARG_NAME =
ARG_EMAIL_PWD =


.PHONY: default change vim emacs emacs-python \
	tmux fonts zsh terminator pictures i3 \
	awesome vundle help

default: change vim emacs tmux fonts zsh terminator i3

change:
ifdef $(ARG_QQ)
	grep -rl "your_id" .* | sed -i "s/your_id/$(ARG_QQ)/g"
endif
ifdef $(ARG_NAME)
	grep -rl "your_name" .* | sed -i "s/your_name/$(ARG_NAME)/g"
endif
ifdef $(ARG_EMAIL_PWD)
	grep -rl "your_email_password" .* | sed -i "s/your_email_password/$(ARG_EMAIL_PWD)/g"
endif

vim:
	cp .vimrc ~/.vimrc

emacs: emacs-python
	git clone https://github.com/syl20bnr/spacemacs.git 
	rm -rf ~/.emacs.d
	cd spacemacs && git checkout be6db59b
	cp -r spacemacs ~/.emacs.d
	cp -r .spacemacs.d ~

emacs-python:
	sudo pip3 install importmagic epc
	sudo pip3 install --upgrade "jedi>=0.9.0" "json-rpc>=1.8.1" "service_factory>=0.1.5"
	sudo pip3 install flake8
	sudo pip3 install autoflake
	sudo pip3 install hy

go:
	go install github.com/go-delve/delve/cmd/dlv@latest
	go install honnef.co/go/tools/cmd/staticcheck@latest
	go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest

tmux:
	cp .tmux.conf* ~

fonts:
	cp -r .fonts ~
	fc-cache -fv


zsh:
	sh -c "$$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
	echo "export TERM=xterm-256color" >> ~/.zshrc

i3:
	cp -r .config/i3 ~/.config

terminator:
	cp -r .config/terminator ~/.config


# options

awesome:
	sudo rm /usr/share/awesom -rf
	sudo cp -r awesome /usr/share/

mutt:
	cp .muttrc ~

pictures: 
	git clone http://github.com/zzx666/wallpapers ~/Pictures/wallpapers

vundle:
	git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

help:
	@echo "please install vim emacs tmux zsh terminator git"
