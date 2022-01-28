# gcalias


## Synopsis

Google Contacts CSV to mutt alias file (Haskell)


## Description

A utility to extract contact records from a CSV export of Google Contacts and
fashion an email aliases file in the format expected by the mutt email client.

Contacts without email addresses are excluded.

For contacts with more than one email, the email label will be included in the
nickname.

Example usage:

    $ gcalias takeout-20220125T221219Z-001.tgz
    alias nicky_remaklus  Nicky Remaklus <GinasMcLaughlinsshat@spotty.gov>
    alias leopoldo_lawonn_home  Leopoldo Lawonn <llawonn@exactingbrine.info>
    alias leopoldo_lawonn_work  Leopoldo Lawonn <centipedes@DoctorslaurelsKhufus.com>
    alias burton_bourke_home  Burton Bourke <fowledfirefighter@transmissible.org>
    alias burton_bourke_zmail  Burton Bourke <reopenscriticized@threshedmiscalled.com>
    alias kelley_bomberger  Kelley Bomberger <modulescapitulation@carrouselsturnpikes.org>
    ...

Redirect this output to where you keep your mutt_aliases file or whatever email
client you use that needs this format.


## Development

### Getting source

Source code is available from github at the
[gcalias](https://github.com/dino-/gcalias) project page.


## Contact

Dino Morelli <dino@ui3.info>
