#### Mineqwaft - A Minecraft server in Lisp
####
#### Copyright (c) 2014, Daniel Parnell <me@danielparnell.com>
####
#### Permission is hereby granted, free of charge, to any person
#### obtaining a copy of this software and associated documentation
#### files (the "Software"), to deal in the Software without
#### restriction, including without limitation the rights to use, copy,
#### modify, merge, publish, distribute, sublicense, and/or sell copies
#### of the Software, and to permit persons to whom the Software is
#### furnished to do so, subject to the following conditions:
####
#### The above copyright notice and this permission notice shall be
#### included in all copies or substantial portions of the Software.
####
#### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#### EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#### MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#### NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#### HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#### WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#### OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
#### DEALINGS IN THE SOFTWARE.
####

# sbcl with no user init script to keep this all self contained
cl := sbcl --userinit /dev/null

# Which ASDF system to load:
system := mineqwaft

# Stardard drivers:
quicklisp-setup := build/scripts/quicklisp-setup.lisp
prop :=  build/scripts/property.lisp
nop := build/scripts/nop.lisp
load :=  build/scripts/load.lisp

# Select which driver to run (load by default).
driver := ${load}

args :=

# A temporary file for passing values around.
tempfile := .tmp

# A command which can be used to get an ASDF system property.
get-property = $(sh build/scripts/run-lisp.sh ${cl} --load ${quicklisp-setup} --eval "(defparameter *driver-system* \"${system}\")" --eval "(defparameter *output-file* \"${tempfile}\")" --eval "(defparameter *output-expression* '$(1))" --load ${prop})

# Get ASDF system properties for the specified system.
define get-properties
	$(eval temp := $$(call get-property,(asdf:component-name *system*)))
	$(eval name := $$(sh cat $${tempfile}))
	$(eval temp := $$(call get-property,(or (asdf:component-property *system* :long-name) (asdf:component-name *system*))))
	$(eval longname := $$(sh cat $${tempfile}))
	$(eval temp := $$(call get-property,(asdf:component-version *system*)))
	$(eval version := $$(sh cat $${tempfile}))
	$(eval temp := $$(call get-property,(asdf:system-description *system*)))
	$(eval description := $$(sh cat $${tempfile}))
	$(eval temp := $$(call get-property,(asdf:component-property *system* :url)))
	$(eval url := $$(sh cat $${tempfile}))
endef

export cl, db, system, driver, name, longname, version, description, url, command

.PHONY: new
new:
	$(MAKE) clean
	$(MAKE) load

.PHONY: load
load:
	${cl} --eval "(defparameter *driver-system* \"${system}\")" --load ${quicklisp-setup} --load ${driver} -- ${args}

.PHONY: shell
shell:
	$(MAKE) driver="${nop}" new

.PHONY: clean
clean:
	rm -rf ${tempfile}
