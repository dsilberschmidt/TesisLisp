# LGC 1989 en la web: SBCL + el servidor minimo en Python (sin dependencias).
FROM ubuntu:24.04
RUN apt-get update \
 && apt-get install -y --no-install-recommends sbcl python3 \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY loader.lisp ./
COPY TESIS/ TESIS/
COPY PROBLEMA/ PROBLEMA/
COPY web/ web/
# Usuario sin privilegios; el codigo es de solo lectura para el.
RUN useradd --system --no-create-home lgc && chmod -R a-w /app
USER lgc
ENV PORT=8080
EXPOSE 8080
CMD ["python3", "web/servidor.py"]
