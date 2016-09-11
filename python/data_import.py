import csv
import datetime

from sqlalchemy import create_engine

from database_setup import City, CityStat, StatIndicator, FlatStat
from database_setup import Session


def parse_float(data):
    #parse data in case it's NA or blank
    if len(str(data)) == 0 or str(data) == "NA":
        return 0
    else:
        return float(data)

def main():
    engine = create_engine('sqlite:///cities.db')
    session = Session(bind=engine)
    with open('CitySet_python.csv') as f:
        reader = csv.reader(f)
        firstline = True
        for line in reader:
            if firstline:
                firstline = False
                # Enter stat indicators
                #### ADD STAT INDICATORS HERE #####
                cinemastat = StatIndicator(name="Cinema Screens")
                educatedpct = StatIndicator(name="Portion Educated")
                session.add_all([cinemastat, educatedpct])
                session.commit()
                continue
            # Read lines now
            # Create a city object, stat indicator, and flat stat
            city = City(name=line[0], long=parse_float(line[-3]), lat=parse_float(line[-4]), area=parse_float(line[9]))
            citystat = CityStat(city=city, population=parse_float(line[-5]), population_updated=datetime.date.today())
            session.add(city)
            session.add(citystat)
            session.commit()
            session.add(citystat)
            session.commit()
            #### ADD FLAT STATS HERE OF WHICH COLUMN HERE ###
            flatstat1 = FlatStat(stat_id=citystat.id, value=parse_float(line[3]))
            flatstat2 = FlatStat(stat_id=citystat.id, value=parse_float(line[6]))
            session.add(flatstat1)
            session.add(flatstat2)
            session.commit()


if __name__ == "__main__":
    main()