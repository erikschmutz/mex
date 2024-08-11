import os
from flask import Flask, request

app = Flask(__name__)

BIN_PATH = os.getenv("CHAPAR_BINARY", "_build/install/default/bin/chapar")

alg = "alg1"
protocol = "udp"
nid = -1
ratio = 10

@app.route('/config')
def config():
    global alg, protocol, nid, ratio

    alg = request.json["alg"]
    protocol = request.json["protocol"]
    nid = request.json["nid"]
    ratio = request.json["ratio"]

    return {"ok": True}

@app.route('/start')
def start():
    os.exec(f"echo '' > time.out")
    os.exec(f"{BIN_PATH} {protocol} {alg} {nid} {ratio} > time.out")
    return {"ok": True}

@app.route('/stop')
def stop():
    os.exec("kill  $(pgrep chapar)")
    return {"ok": True}

@app.route('/collect')
def collect():
    with open("time.txt", "r") as r:
        return {"ok": True, "data": r.read()}

     