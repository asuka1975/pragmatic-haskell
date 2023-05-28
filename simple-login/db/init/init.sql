CREATE TABLE IF NOT EXISTS "user" (
    id       SERIAL       NOT NULL,
    uid      CHAR(36)     NOT NULL,
    email    VARCHAR(255) NOT NULL,
    password CHAR(128)    NOT NULL,
    PRIMARY KEY (uid),
    UNIQUE      (email)
);