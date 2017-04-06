# misc-grad-school

A repository for small projects unrelated to my dissertation


1. [bootstrapped risk diff](http://htmlpreview.github.com/?https://github.com/avonholle/misc-grad-school/blob/master/survival/risk-diff.html)  -- [code](/survival/risk-diff.Rmd)
2. [Sample Venn figure code](/venn/Sample-Venn.Rmd)


# Longleaf and multiple imputation (MI)

1. get and run ssh secure shell (terminal for mac)
2. transfer over your [sas](/mi/wts_mi_code_avh_check.sas) and [shell script](mi/run1.sh) files to your temp directory in longleaf. Mine is /pine/scr/v/o/vonholle/
3. in ssh secure shell (terminal for mac) run [the shell script](mi/run1.sh): 'sbatch run1.sh'
4. check if your job is running: "squeue -u <onyen>"
5. after done running, look at your sas log: 'nano *.log'
6. 

More good info on proc mi at [ats.ucla.edu](http://stats.idre.ucla.edu/sas/seminars/multiple-imputation-in-sas/mi_new_1/)
