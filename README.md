# octoflushow

[Live site on flu-crew.org](http://flu-crew.org/octoflushow/)

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
  cd /etc/apt/sources.list_d      # <= add any sources.list command here instead
  sudo emacs latestRversion.list
  ```
  
  Add the following line, or the appropriated deb command from 
  
  ```
  deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/
  ```
  
  After saving `sources.list`, you'll need to run the following to fetch the latest R version.
  
  ```
  # ! === You will also need to get the R public key from the R website, go to how to install R in ubuntu
  sudo apt-get update
  sudo apt-get install r-base
  sudo apt-get install r-base-dev
  ```

* Start at step 4 of this tutorial [How to host a r-shiny app on aws cloud in 7 steps](https://towardsdatascience.com/how-to-host-a-r-shiny-app-on-aws-cloud-in-7-simple-steps-5595e7885722)

  ```
  # Install R shiny package    =====   Altho better to just start R, and install pkgs using the install.packages("shiny") command
  sudo su — -c “R -e \”install.packages(‘shiny’, repos = ‘http://cran.rstudio.com/')\""     
  
  # Install shiny server
  wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.4.807-amd64.deb
  sudo dpkg -i shiny-server-1.4.4.807-amd64.deb
  cd /srv/shiny-server                                  # <= put your shiny app here
  
  # Start shiny server
  sudo systemctl start shiny-server      
  sudo systemctl status shiny-server                   # check if it's running  
  ```

* Set up a username password account using this tutorial [htpasswd](https://httpd.apache.org/docs/2.4/programs/htpasswd.html)

   ```
   cd /var/www
   sudo htpasswd -c /var/www/.htpasswd [username]      # 1st time, otherwise this will overwrite existing users
   sudo htpasswd /var/www/.htpasswd [username]         # To add more users
   less .htpasswd                                      # check file to make sure username has been added
   ```
   
   If you've changed `000-default.conf` to reference a different password location (`/etc/apache2/.htpasswd`):
   
   ```
   cd /etc/apache2
   sudo htpasswd -c /etc/apache2/.htpasswd [username]      # 1st time, otherwise this will overwrite existing users
   sudo htpasswd /etc/apache2/.htpasswd [username]         # To add more users
   less .htpasswd                                          # check file to make sure username has been added
   ```
   
 * Setup shiny server, or at least reroute the ports to some form of  `https://yourwebsite.com/projectname/`
 
   ```
   cd /etc/apache2/sites-available/
   sudo emacs 000-default.conf                            # <= this is the main file to configure everything
   sudo systemctl restart shiny-server                    # restart server for changes to take effect
   ```
   
   Click through the [documentation](https://httpd.apache.org/docs/2.4/mod/directives.html) for what the tags mean in `000-default.conf` so you can configure it properly.

## Setup AWS to make both octoflushow and nextstrain available:

A host of apache modules need to be enabled for doing reverse proxies and rewritie both headers and html.

```
sudo apt-get install libapache2-mod-proxy-html libxml2-dev
sudo a2enmod proxy
sudo a2enmod proxy_http
sudo a2enmod proxy_wstunnel
sudo a2enmod proxy_html
sudo a2enmod xml2enc
```

The next part is to edit the server configurations at `/etc/apache2/sites-available/000-default.conf`. I will break down the different parts.

The opening virtual host directive should only serve on port 80, or eventually 8080 if you want to allow/force SSL authentication. This is something that should be considered for the future.

```
<VirtualHost *:80>
```

This proxy directive is being used to force authentication to either of the proxied sites. If you need to change the way something functions in the proxies and it is not working in a normal directive, it may need to be added to the proxy directive.

```
        #Zell
        <Proxy *>
             Allow from localhost
             AuthName "Restricted Area"
             AuthType basic
             AuthUserFile /whereeveryouputit/.htpasswd       # <= put your password file here
             require valid-user
        </Proxy>
```

Some of the original parts of the file, specifying the APache2 webroot, which hosts the landing page, and sets the server admin.

```
        # The ServerName directive sets the request scheme, hostname and port that
        # the server uses to identify itself. This is used when creating
        # redirection URLs. In the context of virtual hosts, the ServerName
        # specifies what hostname must appear in the request's Host: header to
        # match this virtual host. For the default virtual host (this file) this
        # value is not decisive as it is used as a last resort host regardless.
        # However, you must set it for any further virtual host explicitly.
        #ServerName www.example.com
        ServerAdmin webmaster@localhost
        DocumentRoot /var/www/html
```

From this point, we begin to set up path rewrite rules. These first two rules automatically add a slash to the end of nextstrain or octoflushow to indicate they are directories, not files.

```
        #Zell
        RedirectMatch permanent ^/octoflushow$ /octoflushow/
        RedirectMatch permanent ^/nextstrain$ /nextstrain/
```

The next set of rewrite rules applies to R-shiny, and allow the service to find resources it needs to function. The rewrite rules were set up in a way to force octoflushow as the default landing page for this tool. It may be possible to traverse up directories to access places the user should not be, please test then review server settings. Please view the apache documentation to understand the P and L flags.

```
        RewriteEngine on
        RewriteCond %{HTTP:Upgrade} =websocket
        RewriteRule /octoflushow/(.*) ws://localhost:3838/sample-apps/octoflushow/$1 [P,L]
        RewriteCond %{HTTP:Upgrade} !=websocket
        RewriteRule /octoflushow/(.*) http://localhost:3838/sample-apps/octoflushow/$1 [P,L]
```

These rewrite rules apply to nextstrain. I have not extensively tested if they are needed, but serve to route `/nextstrain/*` through the nodejs server running at `127.0.0.1:4000`.

```
        RewriteCond %{HTTP:Upgrade} =websocket
        RewriteRule /nextstrain/(.*) ws://localhost:4000/$1 [P,L]
        RewriteCond %{HTTP:Upgrade} !=websocket
        RewriteRule /nextstrain/(.*) http://localhost:4000/$1 [P,L]
```

These lines set up the reverse proxy for R-shiny, defaulting it to the octoflushow directory.

```
        #R-octoflushow reverse proxy
        ProxyPass /octoflushow/ http://localhost:3838/sample-apps/octoflushow/
        ProxyPassReverse /octoflushow/ http://localhost:3838/sample-apps/octoflushow/
```

Multiple reverse proxies were needed for NextStrain. The NodeJS app was designed to assume it was at root. Even if the base html is edited, the auspice javascript files are written to assume that the `/dist/` and `/charon/` directories are located at root. The two extra reverse proxies make the server look at the nodejs server for the resources. A side effect is that the webroot cannot have a `/dist/` or `/charon/` directory.

```
        #Nextstrain reverse proxy
        ProxyPass /nextstrain http://localhost:4000/
        ProxyPassReverse /nextstrain http://localhost:4000/
        ProxyPass /dist http://localhost:4000/dist
        ProxyPassReverse /dist http://localhost:4000/dist
        ProxyPass /charon http://localhost:4000/charon
        ProxyPassReverse /charon http://localhost:4000/charon
        ProxyRequests Off
```

Standard logging information and comments follow.

```
        # Available loglevels: trace8, ..., trace1, debug, info, notice, warn,
        # error, crit, alert, emerg.
        # It is also possible to configure the loglevel for particular
        # modules, e.g.
        #LogLevel info ssl:warn
        #LogLevel info ssl:warn
        ErrorLog ${APACHE_LOG_DIR}/error.log
        CustomLog ${APACHE_LOG_DIR}/access.log combined
        # For most configuration files from conf-available/, which are
        # enabled or disabled at a global level, it is possible to
        # include a line for only one particular virtual host. For example the
        # following line enables the CGI configuration for this host only
        # after it has been globally disabled with "a2disconf".
        #Include conf-available/serve-cgi-bin.conf
```

Next is a location block that was being used to force authentication at all directories. Currently it appears to be defunct, and should be fixed.

```
        <Location "^/$">
            AuthType basic
            AuthName "Restricted Content"
            AuthBasicProvider file
            AuthUserFile "/etc/apache2/.htpasswd"
            Require valid-user
        </Location>
```

This next location directive only applies to the path `/nextstrain/`. It uses the mod_proxy_html module to manually rewrite HTML code before serving to the client. It looks through all tags listed in ProxyHTMLLinks to rewrite the `"/"` path as `"/nextstrain/"`. While this helped fix thei ndex page, pths were being dynamically generated in the auspice javascript that were not captured by the rewrite filters, hence the additional reverse proxies. And ending the virtual host directive

```
        # The javascript is forming url paths, causing everything to fail.
        <Location /nextstrain>
                ProxyHTMLLinks  a          href
                ProxyHTMLLinks  area       href
                ProxyHTMLLinks  link       href
                ProxyHTMLLinks  img        src longdesc usemap
                ProxyHTMLLinks  object     classid codebase data usemap
                ProxyHTMLLinks  q          cite
                ProxyHTMLLinks  blockquote cite
                ProxyHTMLLinks  ins        cite
                ProxyHTMLLinks  del        cite
                ProxyHTMLLinks  form       action
                ProxyHTMLLinks  input      src usemap
                ProxyHTMLLinks  head       profile
                ProxyHTMLLinks  base       href
                ProxyHTMLLinks  script     src for
                ProxyHTMLEnable On
                ProxyHTMLExtended On
                ProxyHTMLURLMap / /nextstrain/
        </Location>
</VirtualHost>
```
