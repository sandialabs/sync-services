import os
import re
import json
import argparse
import requests
import urllib.parse

from flask import Flask, request

app = Flask(__name__)


def listify(expression):
    def _parse_bytes(match):
        return '"<bytes>"'

    expression = re.sub(r'(#u\([\d ]*\))', _parse_bytes, expression)
    expression = re.sub(r'([( ])([a-zA-Z0-9-_*]+)([ )])', r'\1"\2"\3',
                        expression)
    expression = expression.replace(' ', ', ')
    expression = expression.replace('(', '[')
    expression = expression.replace(')', ']')
    return json.loads(expression)


def parse(expression, subpath):
    if expression.startswith('(directory '):
        return (
            'Directory',
            '<ul>{}{}</ul>'.format(
                '<li><a href="..">..</a></li>' if subpath else '',
                ''.join('<li><a href="{}/">{}</a></li>'.format(
                    urllib.parse.quote(x), x)
                        for x in expression[12:-5].split(' ')),
            ),
        )
    elif expression.startswith('(object '):
        return ('Object', f'<div>{expression[7:-1]}</div></br>')
    elif expression.startswith('(nothing '):
        return ('Nothing', '</br>')
    elif expression.startswith('(unknown '):
        return ('Unknown', '</br>')
    else:
        print('Error: ', expression, flush=True)
        return ('Error', '</br>')


def jsonify(expression):
    if expression.startswith('(object '):
        if expression[8] == '"' and expression[-2] == '"':
            return json.dumps(expression[9:-2])
        else:
            return json.dumps(expression[8:-1])
    elif expression.startswith('(directory '):
        return json.dumps(expression[8:-1].split(' '))
    else:
        return json.dumps(None)


def query(expression, is_local=True):
    print(expression)
    if is_local:
        return requests.post(
            f'{app.config["JOURNAL"]}',
            '(*local* "{secret}" {expression})'.format(
                secret=app.config['SECRET'],
                expression=expression,
            )).text
    else:
        return requests.post(
            f'{app.config["JOURNAL"]}',
            expression,
        ).text


def pathify(path):
    return ' '.join(urllib.parse.unquote(x) for x in path.split('/'))


def display(title, content):
    return '''
        <!DOCTYPE html>
            <html lang="en">
            <body style="padding: 0 20px; font-family: 'Consolas'">
                <h2>{title}</h2>
                <div>{content}</div>

            </body>
        </html>
    '''.format(title=title, content=content)


@app.route('/', methods=['GET'])
def handle_home():
    return display(
        'Journal Explorer', ''.join([
            '<li><a href="record/">record</a>',
            '<li><a href="ledger/">ledger</a>',
        ]))


@app.route('/ledger/', methods=['GET'])
def handle_ledger():
    return display(
        'Ledger Interface', ''.join([
            '<li><a href="config/">config</a>',
            '<li><a href="peer/">peer</a>',
            '<li><a href="state/-1/">state</a>',
        ]))


@app.route('/ledger/state/<index>/', methods=['GET'])
@app.route('/ledger/state/<index>/<path:subpath>/', methods=['GET'])
def handle_ledger_state(index, subpath=''):
    result = query(f'(ledger-get ({pathify(subpath)}) {index})')

    if 'Mozilla' not in request.headers.get('User-Agent', ''):
        return jsonify(result)

    sublist = subpath.split('/')
    current = int(query('(ledger-index)'))
    anchor = int(index) if int(index) > 0 else current
    title, content = parse(result, subpath)
    content += '<div><hr><h4>Index</h4>{}</br></br><h4>Path</h4>{}</div>'.format(
        ' | '.join([
            '<a href="/ledger/state/{}/{}">{}</a>'.format(
                i,
                subpath,
                x,
            ) for i, x in [
                (0, 'first'),
                (max(anchor - 100, 0), '<<<'),
                (max(anchor - 10, 0), '<<'),
                (max(anchor - 1, 0), '<'),
                (anchor, anchor),
                (min(anchor + 1, current), '>'),
                (min(anchor + 10, current), '>>'),
                (min(anchor + 100, current), '>>>'),
                (current, 'last'),
            ]
        ]),
        ' > '.join([
            '<a href="/ledger/state/{}/{}">{}</a>'.format(
                index,
                '/'.join(sublist[:i + 1]),
                sublist[i],
            ) for i in range(len(sublist))
        ]),
    )
    return display(title, content)


@app.route('/ledger/peer/', methods=['GET', 'POST'])
def handle_peer():
    if request.method == 'GET':
        return display(
            'Add Peer',
            '''<form method="POST">
                 <label for="name">Name:</label>
                 <input type="text" id="name" name="name" require d>
                 <br>
                 <br>
                 <label for="url">URL:</label>
                 <input type="text" id="url" name="url" required>
                 <br>
                 <br>
                 <input type="submit" value="Submit">
             </form>''',
        )
    else:
        result = query(
            '(ledger-peer! {name} (lambda (m) (sync-remote "{url}" m)))'.
            format(
                name=request.form.get('name'),
                url=request.form.get('url'),
            ))
        return result


@app.route('/ledger/config/', methods=['GET'])
def handle_ledger_config(subpath=''):
    return display(
        'Configuration',
        listify(query('(ledger-config)')),
    )


@app.route('/record/', methods=['GET'])
@app.route('/record/<path:subpath>/', methods=['GET'])
def handle_record(subpath=''):

    QUERY_TEMPLATE = '''
    (*record* "{secret}"
        (lambda (record)
            ((record 'get) '({path}))))
    '''

    result = requests.post(
        f'{app.config["JOURNAL"]}',
        QUERY_TEMPLATE.format(
            secret=app.config['SECRET'],
            path=' '.join(urllib.parse.unquote(x) for x in subpath.split('/')),
        )).text

    return display(*parse(result, subpath))


if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-p',
        '--port',
        type=int,
        default='8080',
        help='Port number to run the Flask app on',
    )
    parser.add_argument(
        '-s',
        '--secret',
        type=str,
        default=None,
        help='A secret variable for the app',
    )
    parser.add_argument(
        '-j',
        '--journal',
        type=str,
        default=None,
        help='A secret variable for the app',
    )

    args = parser.parse_args()

    app.config['JOURNAL'] = args.journal if args.journal else os.getenv(
        'JOURNAL')
    app.config['SECRET'] = args.secret if args.journal else os.getenv('SECRET')

    app.run(host='::', port=args.port)
