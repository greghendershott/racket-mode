.PHONY: doc docs images clean deploy

doc: racket-mode.info racket-mode.html images

docs: doc

clean:
	-rm reference.org
	-rm racket-mode.info
	-rm racket-mode.html

images:
	racket arch-pict.rkt

reference.org: generate.el
	emacs --batch -Q --eval '(progn (add-to-list (quote load-path) "${PWD}/../") (package-initialize))' -l generate.el --funcall 'racket-generate-reference.org'

racket-mode.texi: racket-mode.org reference.org
	emacs --batch -Q -l ox-texinfo racket-mode.org --eval "(setq indent-tabs-mode nil make-backup-files nil)" --funcall org-texinfo-export-to-texinfo

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
	$(aws) s3 cp scenario-0.png   $(dest)/scenario-0.png
	$(aws) s3 cp scenario-1.png   $(dest)/scenario-1.png
	$(aws) s3 cp scenario-2.png   $(dest)/scenario-2.png
	$(aws) s3 cp scenario-3.png   $(dest)/scenario-3.png
	$(aws) s3 cp scenario-4.png   $(dest)/scenario-4.png
	$(aws) cloudfront create-invalidation --distribution-id $(cfid) --paths "/*" > /dev/null
