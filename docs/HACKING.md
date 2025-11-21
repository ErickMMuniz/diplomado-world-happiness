# Docker 

```
docker build -t rstudio-datascience .
```

```
docker run -d \
    -p 8787:8787 \
    -e PASSWORD=modulo8 \
    -v $(pwd)/data:/home/rstudio/data \
    --name rstudio_datascience_app \
    rstudio-datascience
```