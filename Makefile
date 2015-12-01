HEAD = $(shell git rev-parse HEAD)

build:
	docker build -t emacs@git-$(HEAD) .

test: build
	docker run --rm  -v /home/kotfic/.emacs.d:/home/kotfic/remote -it emacs@git-$(HEAD) --debug-init --batch --load /home/kotfic/.emacs.d/init.el

clean:
	docker rmi --force emacs@git-$(HEAD)
