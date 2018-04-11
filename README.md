# mctsGames
monte carlo tree search games
alpha go zero on simpler games with smaller everything


##tips and tricks
###profiling time
stack exec -- *.exe +RTS -sstderr

stack build --profile
stack exec *.exe +RTS -p
more *.prof
###profiling memory
stack exec *.exe +RTS -h -l -L64
stack exec -- hp2ps -c *.hp
gv *.ps

###profiling threads
*.exe +RTS -l
threadscope.linux *.eventlog