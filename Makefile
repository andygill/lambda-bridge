boot::
	cabal configure
	cabal build

LOOPBACK	=	./dist/build/lb_loopback/lb_loopback
SIMULATOR	=	./dist/build/lb_simulator/lb_simulator
BYTE		=	./dist/build/lb_byte/lb_byte
BRIDGE		=	./dist/build/lb_bridge/lb_bridge
BOOTSTRAP	=	./dist/build/lb_bootstrap/lb_bootstrap

test1::
	$(BRIDGE) A B &
	sleep 1
	$(BOOTSTRAP) --slave B &
	sleep 1
	$(BOOTSTRAP) A

test2::
	$(BRIDGE) A B &
	sleep 1
	$(BYTE) A AA &
	sleep 1
	$(BYTE) B BB &
	sleep 1
	$(BOOTSTRAP) --slave BB &
	sleep 1
	$(BOOTSTRAP) AA

clean::	
	killall lb_loopback lb_simulator lb_byte lb_bootstrap lb_bridge
	rm -f loopback byte std
        

