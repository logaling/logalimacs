# Description
Logalimacs is frontend of logaling-command for Ruby gem.

## Usage
put below configuration to your .emacs.
Note that see requirements.

    (add-to-list 'load-path "~/path/to/logalimacs_clone_directory")
    (autoload 'loga-lookup-in-popup "logalimacs" nil t)
    (global-set-key (kbd "C-:") 'loga-lookup-in-popup)

If possible Emacs24, types M-x list-packages, and it can install from items.
If it is only be as where global-set-key~ to be autoloaded.

## Requirements
Logalimacs requires logaling-command.
To install, see below.(gene95:English-Japanese,edict:Japanese-English)

    % gem install logaling-command
    % loga import gene95
    % loga import edict

Put below configuration into your ~/.logaling/config,
If you translate from English to Japanese.

    --glossary your-glossary-name
    --source-language en
    --target-language ja
