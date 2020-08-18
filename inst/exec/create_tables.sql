CREATE TABLE events (
    row_id INTEGER PRIMARY KEY,
    protocol_id INTEGER,
    date_id INTEGER,
    subject_id INTEGER,
    parameter_id INTEGER,
    timestamp REAL,
    event INTEGER,
    parameter REAL
);

CREATE TABLE subjects (
    row_id INTEGER PRIMARY KEY,
    subject_name TEXT,
    species TEXT,
    strain TEXT,
    sex TEXT,
    dob TEXT,
    starting_weight REAL,
    supplier TEXT,
    protocol_name INTEGER,
    environment TEXT,
    FOREIGN KEY (row_id) REFERENCES events (subject_id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE protocols (
    row_id INTEGER PRIMARY KEY,
    protocol_name TEXT UNIQUE,
    protocol_code TEXT,
    start_date TEXT,
    end_date TEXT,
    lab TEXT,
    timestamp_resolution REAL,
    FOREIGN KEY (row_id) REFERENCES events (protocol_id) ON DELETE RESTRICT ON UPDATE CASCADE,
    FOREIGN KEY (row_id) REFERENCES subjects (protocol_id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE eventcodes (
    row_id INTEGER PRIMARY KEY,
    event INTEGER UNIQUE,
    name TEXT UNIQUE,
    FOREIGN KEY (event) REFERENCES events (event) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE parameters (
    row_id INTEGER PRIMARY KEY,
    parameter_id INTEGER,
    parameter_name TEXT,
    FOREIGN KEY (parameter_id) REFERENCES events (parameter_id) ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE dates (
    row_id INTEGER PRIMARY KEY,
    date TEXT UNIQUE,
    FOREIGN KEY (row_id) REFERENCES events (date_id) ON DELETE RESTRICT ON UPDATE CASCADE
);

/* Find better name */
CREATE TABLE scripts (
    row_id INTEGER PRIMARY KEY,
    file TEXT UNIQUE,
    contents TEXT
);
