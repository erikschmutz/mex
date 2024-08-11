import requests

ALGORITHMS = ["alg1", "alg2", "alg3"]
PROTOCOLS = ["tcp", "udp"]
RATIONS = ["10", "20", "30", "40", "50", "60", "70", "80", "90"]
WORKERS = [
    "10.0.0.1:8080",
    "10.0.0.2:8080",
    "10.0.0.3:8080",
    "10.0.0.4:8080",
]
MASTER = "0.0.0.0:8080"

for alg in ALGORITHMS:
    for protocol in PROTOCOLS:
        for ratio in RATIONS:
            # updates the workers to use the latest api
            for nid, worker in enumerate([*WORKERS, MASTER]):
                # stops if already running
                requests.post(
                    f"{worker}/stop", 
                )

                # configures the correct algorithm and protocols
                requests.post(
                    f"{worker}/config", 
                    data={ "alg": alg, "protocol": protocol, "ratio": ratio, "nid": nid }
                )

                # configures the correct algorithm and protocols
                requests.post(
                    f"{worker}/start", 
                )

            data = None
            while not data:
                response = requests.get(
                    f"{worker}/collect", 
                ).json()
                data = response.get(data)
        
            print("{data}")
    


    



