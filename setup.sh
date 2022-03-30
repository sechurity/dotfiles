#!/usr/bin/bash
# Excessive comments for my stupid future self

# Enable and start emacs as a server for emacsclient to connect to 
systemctl --user enable emacs --now

# Append the EDITOR variable setting to ~/.bashrc if it did not exist
# Referenced from: https://arslan.io/2019/07/03/how-to-write-idempotent-bash-scripts/
if ! grep -qF "export EDITOR=.*emacsclient" ~/.bashrc; then
  echo 'export EDITOR="emacsclient -c"' | tee -a ~/.bashrc 
fi

# Reloading shell
. ~/.bashrc
