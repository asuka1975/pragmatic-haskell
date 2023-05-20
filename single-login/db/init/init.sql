CREATE TABLE IF NOT EXISTS users (
    id       SERIAL       NOT NULL,
    email    VARCHAR(255) NOT NULL,
    password CHAR(129)    NOT NULL,
    PRIMARY KEY (id),
    UNIQUE      (email)
);