CREATE TABLE users (
       id INTEGER PRIMARY KEY NOT NULL,
       email TEXT UNIQUE NOT NULL,
       person_id TEXT UNIQUE NOT NULL,
       enabled INTEGER NOT NULL,
       filter TEXT
);
