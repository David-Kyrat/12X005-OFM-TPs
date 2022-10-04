# Git Submodules How-To ?


[Git-Submodules-CheatSheet](https://codex.so/git-submodules-cheatsheet)

---



### Adding Submodules to repo

To add a git repo inside a git repo that will get updated with the rest, we use the command  

```bash
git submodule add <ssh-address> <NameOfWantedFolder>
```

Where `NameOfWantedFolder` is optional and will default to the name of repo.


### Init submodules

After cloning a repository with submodules you need to initialize them. Otherwise you will see empty directories in submodules places.
Git will download submodules recursively.


```bash
git submodule update --init --recursive
```



## Updating Changes Recursively

### Pull updates

Pull submodules by commited state.

```bash
git submodule update --recursive
```

#### Pushing

To recursively push our changes if required, we can use:  

```bash
git push --recurse-submodules=on-demand

```


