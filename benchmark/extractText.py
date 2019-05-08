#!/usr/bin/python3

import json

with open('AllCards.json') as allCards:
    data = json.load(allCards)
    for name, body in data.items():
        try:
            text = body['text']
        except KeyError:
            continue
        print(name.encode('utf-8') + " " + text.encode('utf-8'))
