from sqlalchemy import create_engine
from sqlalchemy import Column, Integer, String, Float, Text, DateTime
from sqlalchemy import ForeignKey
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker

Base = declarative_base()
Session = sessionmaker()


class City(Base):
    __tablename__ = "cities"

    id = Column(Integer, primary_key=True)
    name = Column(String)
    long = Column(Float)
    lat = Column(Float)
    profile = Column(Text)
    area = Column(Float)

    def __repr__(self):
        return self.name


class CityStat(Base):
    __tablename__ = "city_stats"

    id = Column(Integer, primary_key=True)
    city_id = Column(Integer, ForeignKey('cities.id'))
    population = Column(Integer)
    population_updated = Column(DateTime)

    def __repr__(self):
        return str(self.city_id) + " with population " + str(self.population)


class StatIndicator(Base):
    __tablename__ = "stat_indicators"

    id = Column(Integer, primary_key=True)
    name = Column(String)
    category = Column(String)
    tags = Column(String)  # Probably needs to be array, think about how to make more sophisticated later

    def __repr__(self):
        return self.name + " as a stat indicator."


class FlatStat(Base):
    __tablename__ = "flat_stats"

    id = Column(Integer, primary_key=True)
    stat_id = Column(Integer, ForeignKey('stat_indicators.id'))
    value = Column(Float)

    def __repr__(self):
        return str(self.stat_id) + " with value " + str(self.value)


def import_data(filename, engine):
    # TODO: implement read from file.
    # Assume we have that part completed. Make objects and insert into db
    session = Session()
    session.add_all([])
    session.commit()

def main():
    # Set up database here
    engine = create_engine('sqlite:///cities.db')  # or use sqlite:///memory for reseting every time.
    Base.metadata.create_all(engine)
    Session.configure(bind=engine)
    #import_data()

if __name__ == "__main__":
    main()
