echo stop shiny server service
systemctl stop shiny-server.service

echo upgrading changes
rm -r /srv/shiny-server/whatsappAnalysis
cp -r /home/ian/Documentos/RCode/whatsappAnalysis /srv/shiny-server/

echo starting service
systemctl start shiny-server.service
