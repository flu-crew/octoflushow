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

## Updating octoflushow on AWS

```
cd github/octoflushow   # This will probably be in a github folder, somewhere in home directory
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

## AWS Initial Setup

* Ubuntu defaults to R v3.4 but you will need >v3.5 for several R packages to install. So follow instructions here [Ubuntu packages for R](http://lib.stat.cmu.edu/R/CRAN/bin/linux/ubuntu/)

  ```
  cd /etc/apt
  sudo emacs sources.list
  ```
  
  Add the following line, or the appropriated deb command from 
  
  ```
  deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/
  ```
  
  After saving `sources.list`, you'll need to run the following to fetch the latest R version.
  
  ```
  sudo apt-get update
  sudo apt-get install-rbase
  sudo apt-get install r-base-dev
  ```

* Start at step 4 of this tutorial [How to host a r-shiny app on aws cloud in 7 steps](https://towardsdatascience.com/how-to-host-a-r-shiny-app-on-aws-cloud-in-7-simple-steps-5595e7885722)

* Set up a username password account using this tutorial [htpasswd](https://httpd.apache.org/docs/2.4/programs/htpasswd.html)

   ```
   cd /var/www
   sudo htpasswd -c /var/www/.htpasswd [username]      # 1st time, otherwise this will overwrite existing users
   sudo htpasswd /var/www/.htpasswd [username]         # To add more users
   less .htpasswd                                      # check file to make sure username has been added
   ```
   
 * Setup shiny server, or at least reroute the ports to some form of  `https://yourwebsite.com/projectname/`
 
   ```
   cd /etc/apache2/sites-available/
   sudo emacs 000-default.conf                            # <= this is the main file to configure everything
   sudo systemctl restart shiny-server                    # restart server for changes to take effect
   ```
   
   Click through the [documentation](https://httpd.apache.org/docs/2.4/mod/directives.html) for what the tags mean in `000-default.conf` so you can configure it properly.
   
