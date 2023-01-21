cd ~\OneDrive\Documents\BA3\OFM-12X015\cleanTPs\
git submodule update --init --recursive

ls -Directory | % {cd $_;  git restore . --staged; git restore .; cd .. }

