# Set default values if not specified
PREFIX ?= /usr
infodir ?= $(PREFIX)/share/info

.PHONY: docs install-docs

# Build the info documentation
docs: dired-image-thumbnail.info

dired-image-thumbnail.info: dired-image-thumbnail.texi
	makeinfo --no-split $< -o $@

# Install the info documentation
install-docs: docs
	@echo "Installing info documentation..."
	install -d $(DESTDIR)$(infodir)
	install -m 644 dired-image-thumbnail.info $(DESTDIR)$(infodir)/
	install-info --info-dir=$(DESTDIR)$(infodir) dired-image-thumbnail.info
