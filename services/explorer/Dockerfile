FROM python:3.11.9-alpine3.19

ARG pip_args

WORKDIR /srv

COPY requirements.txt .
RUN pip install -r requirements.txt

COPY service.py .
CMD python service.py -p 80