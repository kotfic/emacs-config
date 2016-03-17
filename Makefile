HEAD = $(shell git rev-parse HEAD)

build:
	docker build -t emacs_git_$(HEAD) .

ubuntu: build
	docker run --rm -it emacs_git_$(HEAD) emacs --debug-init

travis: build
	docker run --rm -it emacs_git_$(HEAD) emacs --batch --load /home/kotfic/.emacs.d/init.el

clean:
	docker rmi --force emacs_git_$(HEAD)
