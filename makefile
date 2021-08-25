# This file is part of Factory.
#
# Factory is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Factory is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Factory.  If not, see <http://www.gnu.org/licenses/>.

.PHONY: hlint test prof cabalCheck sdist findOmissions haddock graphmod

PACKAGE_NAME	= factory
SHELL		= /bin/bash
GHC_OPTIONS	= --ghc-options='-j'
BIN_DIR		= $$HOME/.local/bin/

# Build hlint.
$(BIN_DIR)/hlint:
	@stack install hlint

# Check for lint.
hlint: $(BIN_DIR)/hlint
	@$@ src-*

# Compile & run the test-suite.
test:
	stack '$@' $(GHC_OPTIONS)

# Profile.
prof:
	@stack install --library-profiling --executable-profiling $(GHC_OPTIONS)

# Check the cabal-file.
cabalCheck:
	@cabal check

# Package for upload to Hackage.
sdist:
	@cabal '$@'

# Find source-files missing from the distribution.
findOmissions: sdist
	@diff <(find src-* -type f -name '*.hs' | sed 's!^\./!!' | sort) <(tar -ztf dist*/sdist/$(PACKAGE_NAME)-*.tar.gz | grep '\.hs$$' | grep -v 'Setup.hs' | sed 's!^$(PACKAGE_NAME)-[0-9.]*/!!' | sort)

# Install this product.
$(BIN_DIR)/$(PACKAGE_NAME):
	@stack install $(GHC_OPTIONS)

# Build the source-code documentation.
haddock:
	@stack '$@' --no-$@-deps $(GHC_OPTIONS)

# Show module-dependency graph.
graphmod:
	@$@ --graph-dim='40,24' -i 'src-exe' -i 'src-lib' Main | tred | dot -Tsvg | display

