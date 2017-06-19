Cyber Squad
===========

**Machine Learning** and **Data Mining** algorithms for **Cyber Security** implemented in **Haskell**

Tentative Style Guide
------------------------

see [here](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)

How to use
----------

### Getting

0. Don't be on Windows (or use a virtual machine or emulator)
1. Clone from link provided on github
2. Name folder that it will be placed in `cyber` just for simplicity (it doesn't have to be)
3. get stack ([instructions](https://docs.haskellstack.org/en/stable/README/))
4. run `stack setup` on project

### Building and running

1. Run `stack solver --update-config`
2. Run `stack build`
3. Run `stack exec cyber-exe`

### Testing

2. Run `stack test`

### Compile Documentation

1. Run `stack haddock`

Database
--------

### DARPA File System Configuration

please configure the database files in a folder with these names

- DARPA 1999 - Week 1 - Monday - inside.tcpdump
- DARPA 1999 - Week 1 - Monday - outside.tcpdump
- DARPA 1999 - Week 1 - Tuesday - inside.tcpdump
- DARPA 1999 - Week 1 - Tuesday - outside.tcpdump
- DARPA 1999 - Week 1 - Wednesday - inside.tcpdump
- DARPA 1999 - Week 1 - Wednesday - outside.tcpdump
- DARPA 1999 - Week 1 - Thursday - inside.tcpdump
- DARPA 1999 - Week 1 - Thursday - outside.tcpdump
- DARPA 1999 - Week 1 - Friday - inside.tcpdump
- DARPA 1999 - Week 1 - Friday - outside.tcpdump

**FIXME**: for now, only having exacly these files and all of these files will cause the `getDatasetReference` function from DARPA.Access to verify the file path given.
