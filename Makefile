HEAD = $(shell git rev-parse HEAD)

build:
	docker build -t emacs@git-$(HEAD) .

ubuntu: build
	docker run --rm -it emacs@git-$(HEAD) emacs --debug-init

travis: build
	docker run --rm -it emacs@git-$(HEAD) emacs --batch --load ~/.emacs.d/init.el

clean:
	docker rmi --force emacs@git-$(HEAD)
