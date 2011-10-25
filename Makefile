boot::
	cabal configure
	cabal build

LOOPBACK	=	./dist/build/lb_loopback/lb_loopback
SIMULATOR	=	./dist/build/lb_simulator/lb_simulator
BYTE		=	./dist/build/lb_byte/lb_byte
test::
	$(LOOPBACK) --debug loopback &
	sleep 1
	$(SIMULATOR) byte loopback byte &
	sleep 1
	$(BYTE) byte std

clean::	
	killall lb_loopback lb_simulator lb_byte
	rm -f loopback byte std
        

