FC = gfortran
FCFLAGS = -Jmod -Imod -O2
FLFLAGS = -s
PROG = bin/f95stats.cgi
MOD = obj/stats_module.o
OBJ = obj/cgistats.o obj/compute.o obj/report.o
DIRS = obj mod bin

$(PROG): $(OBJ) $(MOD)
	$(FC) $(FLFLAGS) -o $@ $^

obj/stats_module.o: src/stats_module.f95
	$(FC) -o $@ -c $(FCFLAGS) $<

obj/compute.o: src/compute.f95 $(MOD)
	$(FC) -o $@ -c $(FCFLAGS) $<

obj/report.o: src/report.f95 $(MOD)
	$(FC) -o $@ -c $(FCFLAGS) $<

obj/cgistats.o: src/cgistats.f95 $(MOD)
	$(FC) -o $@ -c $(FCFLAGS) $<

install:
	sudo cp src/f95stats.html /srv/httpd/htdocs
	sudo chown apache:apache /srv/httpd/htdocs/f95stats.html
	sudo cp src/gradienttable.css /srv/httpd/htdocs/includes
	sudo chown apache:apache /srv/httpd/htdocs/includes/gradienttable.css
	sudo cp bin/f95stats.cgi /srv/httpd/cgi-bin
	sudo chown apache:apache /srv/httpd/cgi-bin/f95stats.cgi

.PHONY : clean
clean:
	$(RM) $(PROG) $(OBJ) $(MOD) mod/*

$(shell mkdir -p $(DIRS))
