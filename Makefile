HEAD = $(shell git rev-parse HEAD)

build:
	docker build -t emacs@git-$(HEAD) .

ubuntu: build
	docker run --rm -it emacs@git-$(HEAD) emacs --debug-init

travis: build
	sudo -u kotfic docker run --rm -it emacs@git-$(HEAD) emacs --batch --load /home/kotfic/.emacs.d/init.el

clean:
	docker rmi --force emacs@git-$(HEAD)
