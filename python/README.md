Guide to using this program:

To get the database setup, run database_setup.py. This will provide a SQLite DB with the a manageable structure.
To get test data in there, run data_import.py. This will get some dummy data from CitySet imported in.
These files should only be run once! Otherwise, delete the database, and run both files again.

To make customizations to the data pull, look at the csv file, and see which column the data is in. Then add it to the list.

TODO:
- script to call dbscan cluster simulation