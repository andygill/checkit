GHCi=$(GHC69) --interactive
#GHCi=ghci
boot::
#	$(GHCi) -XFlexibleInstances -XFlexibleContexts Hack.hs
	$(GHCi) Main.hs
#	$(GHCi) -XOverlappingInstances -XFlexibleInstances -XFlexibleContexts Stack.hs 
#	$(GHCi) -XFlexibleInstances -XFlexibleContexts ArgHack.hs -fglasgow-exts -fallow-overlapping-instances
# -fallow-incoherent-instances


boot2::
	$(GHC69) --make Main.hs -O2 -threaded -fvia-C

clean::
	find . -name '*.o' -print | xargs rm -f
