.PHONY: doc docs images clean deploy

doc: images racket-mode.info racket-mode.html

docs: doc

clean:
	-rm scenario*.png
	-rm scenario*.svg
	-rm reference.org
	-rm racket-mode.info
	-rm racket-mode.html

images:
	racket arch-pict.rkt

reference.org: generate.el
	emacs --batch -Q --eval '(progn (add-to-list (quote load-path) "${PWD}/../") (package-initialize))' -l generate.el --funcall 'racket-generate-reference.org'

racket-mode.texi: racket-mode.org reference.org
	emacs --batch -Q -l ox-texinfo racket-mode.org --eval "(setq indent-tabs-mode nil make-backup-files nil org-src-preserve-indentation t)" --funcall org-texinfo-export-to-texinfo

racket-mode.info: racket-mode.texi
	makeinfo --no-split $< -o $@

racket-mode.html: racket-mode.texi
	makeinfo --html --no-split --no-headers --no-number-sections --css-ref='racket-mode.css' $<


######################################################################
# S3 bucket deploy

aws  := aws --profile greg
dest := s3://www.racket-mode.com
cfid := E1OG6O4MCHIO1Q

.PHONY: deploy

deploy: racket-mode.html racket-mode.css images
	$(aws) s3 cp racket-mode.html $(dest)/index.html
	$(aws) s3 cp racket-mode.css  $(dest)/racket-mode.css
	$(aws) s3 cp scenario-0.svg   $(dest)/scenario-0.svg
	$(aws) s3 cp scenario-1.svg   $(dest)/scenario-1.svg
	$(aws) s3 cp scenario-2.svg   $(dest)/scenario-2.svg
	$(aws) s3 cp scenario-3.svg   $(dest)/scenario-3.svg
	$(aws) s3 cp scenario-4.svg   $(dest)/scenario-4.svg
	$(aws) cloudfront create-invalidation --distribution-id $(cfid) --paths "/*" > /dev/null
