# -*- mode: ruby; coding: utf-8 -*-

# This program is Rakefile for marmalade(Repository site for Emacs lisp). 
# You can possible to create package-directory for marmalade.

# Input your configuration
PACKAGE_NAME = "logalimacs"
PACKAGE_VERSION = "0.0.2"
REQUIREMENTS = ["logalimacs.el", "logalimacs-config.el",
                "logalimacs-rurema.el"]
DESCRIPTION = "Front-end of logaling-command for ruby gem"

# Require list structure for Emacs lisp
# @todo refactor to support against multiple list
DEPENDENCIES = '\'((popwin "0.4"))'

# Decided at automatic
PKG_EL = "#{PACKAGE_NAME}-pkg.el"
PKG_EL_CONTENT = <<-ELISP
(define-package "#{PACKAGE_NAME}" "#{PACKAGE_VERSION}" "#{DESCRIPTION}" #{DEPENDENCIES})
ELISP
MARMALADE_PACKAGE_NAME = "#{PACKAGE_NAME}-#{PACKAGE_VERSION}"

#--rakefile--
task :default => :package

desc "Package #{PACKAGE_NAME}"
task :package => :bundle do
  sh("tar", "cvf", "#{MARMALADE_PACKAGE_NAME}.tar", MARMALADE_PACKAGE_NAME)
end

desc "Bundle #{PACKAGE_NAME}"
task :bundle => "Rakefile" do
  rm_rf(MARMALADE_PACKAGE_NAME)
  mkdir(MARMALADE_PACKAGE_NAME)
  REQUIREMENTS.each do |file|
    cp(file, MARMALADE_PACKAGE_NAME)
  end
  open("#{MARMALADE_PACKAGE_NAME}/#{PKG_EL}", "w") do |content|
    content.print(PKG_EL_CONTENT)
  end
end
