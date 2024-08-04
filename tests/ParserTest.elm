module ParserTest exposing (agencies, areas, calendar_dates, calendars, frequencies, levels, location_groups, networks, pathways, route_networks, routes, shape_points, stop_areas, stop_times, stops, trips)

import Expect
import Parser.OfTokens as Parser exposing (Parser)
import Parser.Token exposing (Token)
import Parser.Tokenizer
import Rope
import SQLite.Statement as Statement
import Test exposing (Test, test)
import TestCommon exposing (testOutputRow, tokenizedToString)


agencies : Test
agencies =
    justParseStatement "CREATE TABLE agencies"
        """CREATE TABLE agencies (
            feed TEXT NOT NULL,
            agency_id TEXT NOT NULL,
            agency_name TEXT NOT NULL,
            agency_url TEXT NOT NULL,
            agency_timezone TEXT NOT NULL,
            agency_lang TEXT,
            agency_phone TEXT,
            agency_fare_url TEXT,
            agency_email TEXT,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                agency_id
            )
        ) STRICT"""


stops : Test
stops =
    justParseStatement "CREATE TABLE stops"
        """CREATE TABLE stops (
            feed TEXT NOT NULL,
            stop_id TEXT NOT NULL,
            stop_code TEXT,
            stop_name TEXT,
            tts_stop_name TEXT,
            stop_desc TEXT,
            stop_lat REAL,
            stop_lon REAL,
            zone_id TEXT,
            stop_url TEXT,
            location_type INTEGER,
            parent_station TEXT,
            stop_timezone TEXT,
            wheelchair_boarding INTEGER,
            level_id TEXT,
            platform_code TEXT,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                stop_id
            ),
            CONSTRAINT fk_stops FOREIGN KEY (
                feed,
                parent_station
            ) REFERENCES stops (
                feed,
                stop_id
            ),
            CONSTRAINT fk_levels FOREIGN KEY (
                feed,
                level_id
            ) REFERENCES levels (
                feed,
                level_id
            )
        ) STRICT"""


routes : Test
routes =
    justParseStatement "CREATE TABLE routes"
        """CREATE TABLE routes (
            feed TEXT NOT NULL,
            route_id TEXT NOT NULL,
            agency_id TEXT,
            route_short_name TEXT,
            route_long_name TEXT,
            route_desc TEXT,
            route_type INTEGER NOT NULL,
            route_url TEXT,
            route_color TEXT,
            route_text_color TEXT,
            route_sort_order INTEGER,
            continuous_pickup INTEGER,
            continuous_drop_off INTEGER,
            network_id TEXT,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                route_id
            ),
            CONSTRAINT fk_agencies FOREIGN KEY (
                feed,
                agency_id
            ) REFERENCES agencies (
                feed,
                agency_id
            )
        ) STRICT"""


trips : Test
trips =
    justParseStatement "CREATE TABLE trips"
        """CREATE TABLE trips (
            feed TEXT NOT NULL,
            route_id TEXT NOT NULL,
            service_id TEXT NOT NULL,
            trip_id TEXT NOT NULL,
            trip_headsign TEXT,
            trip_short_name TEXT,
            direction_id INTEGER,
            block_id TEXT,
            shape_id TEXT,
            wheelchair_accessible INTEGER,
            bikes_allowed INTEGER,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                trip_id
            ),
            CONSTRAINT fk_routes FOREIGN KEY (
                feed,
                route_id
            ) REFERENCES routes (
                feed,
                route_id
            )
        ) STRICT"""


stop_times : Test
stop_times =
    justParseStatement "CREATE TABLE stop_times"
        """CREATE TABLE stop_times (
            feed TEXT NOT NULL,
            trip_id TEXT NOT NULL,
            arrival_time TEXT,
            departure_time TEXT,
            stop_id TEXT,
            location_group_id TEXT,
            location_id TEXT,
            stop_sequence INTEGER NOT NULL,
            stop_headsign TEXT,
            start_pickup_drop_off_window TEXT,
            end_pickup_drop_off_window TEXT,
            pickup_type INTEGER,
            drop_off_type INTEGER,
            continuous_pickup INTEGER,
            continuous_drop_off INTEGER,
            shape_dist_traveled REAL,
            timepoint INTEGER,
            pickup_booking_rule_id TEXT,
            drop_off_booking_rule_id TEXT,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                trip_id,
                stop_sequence
            ),
            CONSTRAINT fk_trips FOREIGN KEY (
                feed,
                trip_id
            ) REFERENCES trips (
                feed,
                trip_id
            ),
            CONSTRAINT fk_stops FOREIGN KEY (
                feed,
                stop_id
            ) REFERENCES stops (
                feed,
                stop_id
            ),
            CONSTRAINT fk_location_groups FOREIGN KEY (
                feed,
                location_group_id
            ) REFERENCES location_groups (
                feed,
                location_group_id
            )
        ) STRICT"""


calendars : Test
calendars =
    justParseStatement "CREATE TABLE calendars"
        """CREATE TABLE calendars (
            feed TEXT NOT NULL,
            service_id TEXT NOT NULL,
            monday INTEGER NOT NULL,
            tuesday INTEGER NOT NULL,
            wednesday INTEGER NOT NULL,
            thursday INTEGER NOT NULL,
            friday INTEGER NOT NULL,
            saturday INTEGER NOT NULL,
            sunday INTEGER NOT NULL,
            start_date INTEGER NOT NULL,
            end_date INTEGER NOT NULL,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                service_id
            )
        ) STRICT"""


calendar_dates : Test
calendar_dates =
    justParseStatement "CREATE TABLE calendar_dates"
        """CREATE TABLE calendar_dates (
            feed TEXT NOT NULL,
            service_id TEXT NOT NULL,
            date INTEGER NOT NULL,
            exception_type INTEGER NOT NULL,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                service_id,
                date
            )
        ) STRICT"""


areas : Test
areas =
    justParseStatement "CREATE TABLE areas"
        """CREATE TABLE areas (
            feed TEXT NOT NULL,
            area_id TEXT NOT NULL,
            area_name TEXT,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                area_id
            )
        ) STRICT"""


stop_areas : Test
stop_areas =
    justParseStatement "CREATE TABLE stop_areas"
        """CREATE TABLE stop_areas (
            feed TEXT NOT NULL,
            area_id TEXT NOT NULL,
            stop_id TEXT NOT NULL,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                area_id,
                stop_id
            ),
            CONSTRAINT fk_areas FOREIGN KEY (
                feed,
                area_id
            ) REFERENCES areas (
                feed,
                area_id
            ),
            CONSTRAINT fk_stops FOREIGN KEY (
                feed,
                stop_id
            ) REFERENCES stops (
                feed,
                stop_id
            )
        ) STRICT"""


networks : Test
networks =
    justParseStatement "CREATE TABLE networks"
        """CREATE TABLE networks (
            feed TEXT NOT NULL,
            network_id TEXT NOT NULL,
            network_name TEXT,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                network_id
            )
        ) STRICT"""


route_networks : Test
route_networks =
    justParseStatement "CREATE TABLE route_networks"
        """CREATE TABLE route_networks (
            feed TEXT NOT NULL,
            network_id TEXT NOT NULL,
            route_id TEXT NOT NULL,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                route_id
            ),
            CONSTRAINT fk_networks FOREIGN KEY (
                feed,
                network_id
            ) REFERENCES networks (
                feed,
                network_id
            ),
            CONSTRAINT fk_routes FOREIGN KEY (
                feed,
                route_id
            ) REFERENCES routes (
                feed,
                route_id
            )
        ) STRICT"""


shape_points : Test
shape_points =
    justParseStatement "CREATE TABLE shape_points"
        """CREATE TABLE shape_points (
            feed TEXT NOT NULL,
            shape_id TEXT NOT NULL,
            shape_pt_lat REAL NOT NULL,
            shape_pt_lon REAL NOT NULL,
            shape_pt_sequence INTEGER NOT NULL,
            shape_dist_traveled REAL,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                shape_id,
                shape_pt_sequence
            )
        ) STRICT"""


frequencies : Test
frequencies =
    justParseStatement "CREATE TABLE frequencies"
        """CREATE TABLE frequencies (
            feed TEXT NOT NULL,
            trip_id TEXT NOT NULL,
            start_time TEXT NOT NULL,
            end_time TEXT NOT NULL,
            headway REAL NOT NULL,
            exact_times INTEGER,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                trip_id,
                start_time
            ),
            CONSTRAINT fk_trips FOREIGN KEY (
                feed,
                trip_id
            ) REFERENCES trips (
                feed,
                trip_id
            )
        ) STRICT"""


pathways : Test
pathways =
    justParseStatement "CREATE TABLE pathways"
        """CREATE TABLE pathways (
            feed TEXT NOT NULL,
            pathway_id TEXT NOT NULL,
            from_stop_id TEXT NOT NULL,
            to_stop_id TEXT NOT NULL,
            pathway_mode INTEGER NOT NULL,
            is_bidirectional INTEGER NOT NULL,
            length REAL,
            traversal_time REAL,
            stair_count INTEGER,
            max_slope REAL,
            min_width REAL,
            signposted_as TEXT,
            reversed_signposted_as TEXT,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                pathway_id
            ),
            CONSTRAINT fk_stops FOREIGN KEY (
                feed,
                from_stop_id
            ) REFERENCES stops (
                feed,
                stop_id
            ),
            CONSTRAINT fk_stops FOREIGN KEY (
                feed,
                to_stop_id
            ) REFERENCES stops (
                feed,
                stop_id
            )
        ) STRICT"""


levels : Test
levels =
    justParseStatement "CREATE TABLE levels"
        """CREATE TABLE levels (
            feed TEXT NOT NULL,
            level_id TEXT NOT NULL,
            level_index REAL NOT NULL,
            level_name TEXT,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                level_id
            )
        ) STRICT"""


location_groups : Test
location_groups =
    justParseStatement "CREATE TABLE location_groups"
        """CREATE TABLE location_groups (
            feed TEXT NOT NULL,
            location_group_id TEXT NOT NULL,
            location_group_name TEXT,
            CONSTRAINT pk PRIMARY KEY (
                feed,
                location_group_id
            )
        ) STRICT"""


justParseStatement : String -> String -> Test
justParseStatement label input =
    testParse label Statement.parser Statement.toRope input Nothing


testParse :
    String
    -> Parser Token a
    -> (a -> Rope.Rope String)
    -> String
    -> Maybe a
    -> Test
testParse label parser toRope input value =
    test label <|
        \_ ->
            case Parser.Tokenizer.tokenizer input of
                Ok tokenized ->
                    let
                        parsed : Result (List (Parser.DeadEnd Token)) a
                        parsed =
                            tokenized
                                |> Parser.run (parser |> Parser.skip Parser.end)
                    in
                    case value of
                        Just v ->
                            if parsed == Ok v then
                                Expect.pass

                            else
                                [ testOutputRow label input
                                , testOutputRow "Tokenized" (tokenizedToString tokenized)
                                , testOutputRow "Parsed" (TestCommon.parseResultToString toRope input parsed)
                                ]
                                    |> String.join "\n"
                                    |> Expect.fail

                        Nothing ->
                            case parsed of
                                Ok _ ->
                                    Expect.pass

                                Err _ ->
                                    [ testOutputRow label input
                                    , testOutputRow "Tokenized" (tokenizedToString tokenized)
                                    , testOutputRow "Parsed" (TestCommon.parseResultToString toRope input parsed)
                                    ]
                                        |> String.join "\n"
                                        |> Expect.fail

                Err ( location, e ) ->
                    let
                        lines : List String
                        lines =
                            String.split "\n" input
                    in
                    [ testOutputRow label input
                    , testOutputRow "Tokenized" (TestCommon.viewProblem lines location.row location.column [ e ])
                    ]
                        |> String.join "\n"
                        |> Expect.fail
