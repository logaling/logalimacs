# -*- coding: utf-8 -*-

# this program is Rakefile for marmalade(Repository site for Emacs lisp). 
# you can possible to create package-directory for marmalade

# Input your configuration
PACKAGE_NAME = "logalimacs"
VERSION = "0.0.2"
REQUIREMENTS = ["logalimacs.el", "logalimacs-config.el",
                "logalimacs-rurema.el"]
DESCRIPTION = "Front-end of logaling-command for ruby gem"

# require list structure for Emacs lisp
# @todo refactor to support against multiple list
DEPENDING_ON = '\'((popwin "0.4"))'


# Decided at automatic
NAME_OF_PKG_EL = PACKAGE_NAME.to_s + "-pkg.el"
CONTENT_OF_PKG_EL = "(define-package \"#{PACKAGE_NAME}\" \"#{VERSION}\"
\"#{DESCRIPTION}\" #{DEPENDING_ON})"
MARMALADE_PKG_NAME = PACKAGE_NAME + "-" + VERSION

#--rakefile--
task :default => :packaging_for_marmalade

task :packaging_for_marmalade => :create_tar do
end

task :make_pkg_el do
  `rm -fr #{file_name = NAME_OF_PKG_EL}`
  open(NAME_OF_PKG_EL, "w") do |content|
    content.print(CONTENT_OF_PKG_EL)
  end
end

task :file_handler => :make_pkg_el do
  `rm -fr #{MARMALADE_PKG_NAME}`
  `mkdir #{MARMALADE_PKG_NAME}`
  REQUIREMENTS.each do |file|
    `cp #{file} #{MARMALADE_PKG_NAME}`
  end
  `cp #{NAME_OF_PKG_EL} #{MARMALADE_PKG_NAME}`
end

task :create_tar => :file_handler do
  `tar cvf #{MARMALADE_PKG_NAME}.tar #{MARMALADE_PKG_NAME}`
end
