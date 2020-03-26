# octoFLU Show

## Installation

The source code for the app is available on github and can be installed with `devtools`:

```
devtools::install_github("flu-crew/octoflushow", auth_token="yourtokenhere")

octoflushow::launch_app()
```

or 

```
devtools::document()
devtools::install()
octoflushow::launch_app()
```

## AWS site

```
cd octoflushow   # This might be in home folder or /srv/ folder somewhere
git pull
sudo R
```

From within the R environment
```
devtools::install()
# When it asks if you want to update packages, choose "none" (3).

# Check if it installed correctly
octoflushow::launch_app()
#ctrl-d to stop 
```

From back in the shell

```
sudo systemctl restart shiny-server
```

Check the website address see if site is working.
