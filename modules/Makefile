clean:
	rm -rf *.md *.html *.pdf cache/ figure/

%.html: %.Rmd
	./make_slides $(basename $(@))

all: clean 1 2 3 4 5 6 7 8 9 10 11



1: 1_nimble_background.html
2: 2_basic_use.html
3: 3_nimble_bugs.html
4: 4_mcmc_concepts.html
5: 5_customizing_mcmc.html
6: 6_variable_selection.html
7: 7_user_dist.html
8: 8_exercises.html
9: 9_using_models.html
10: 10_user_sampler.html
11: 11_wrapup.html

