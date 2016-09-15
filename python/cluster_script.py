from database_setup import Session, FlatStat
from sqlalchemy import create_engine

from sklearn.cluster import DBSCAN
import numpy as np


def transform_to_np_array(query):
    cinema_arr = []
    educated_arr = []
    for row in query:
        # Have a flat stat instance. Transform to array here.
        # As a placeholder, we will use the values of cinema as x axis, and uneducated as y
        # Probably wrong though. Need to discuss what's the general process here.
        if row.stat_id == 1:
            cinema_arr.append(row.value)
        else:
            educated_arr.append(row.value*100)

    return np.array(list(zip(cinema_arr, educated_arr)))

def main():
    """
    We want to grab data from the db, put it into coordinate style for scipy, then run our cluster on them
    The coordinate transform from the db is the reusable part.
    What format do we need as output is the question. And how do we cluster on multiple dimensions?
    Also worth setting up the pipeline to graphing it.
    """
    # First grab data from db
    engine = create_engine("sqlite:///cities.db")
    session = Session(bind=engine)
    # Grab all data from flat stats table
    query = session.query(FlatStat).all()

    X = transform_to_np_array(query)

    # Run dbscan now
    results = DBSCAN(eps=2, min_samples=10).fit(X)

    # TODO: get matplotlib in here, but for now, just print all properties of dbscan
    print(results.__dict__)


if __name__ == "__main__":
    main()