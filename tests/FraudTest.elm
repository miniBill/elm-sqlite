module FraudTest exposing (..)

{-| Code comes from <https://analytics.fixelsmith.com/posts/sql-fraud-patterns/>
-}

import ParserTest exposing (justParseStatement_)
import Test exposing (Test)


velocity : Test
velocity =
    """
    SELECT
        cardholder_id,
        date_trunc('hour', timestamp) AS hour_bucket,
        count(*) AS tx_count,
        min(timestamp) AS first_tx,
        max(timestamp) AS last_tx
    FROM transactions
    WHERE timestamp >= current_date - INTERVAL '30 days'
    GROUP BY 1, 2
    HAVING count(*) > 10;
    """ |> justParseStatement_ "Velocity"


velocitySlidingWindow : Test
velocitySlidingWindow =
    """
    SELECT
        cardholder_id,
        timestamp,
        count(*) OVER (
            PARTITION BY cardholder_id
            ORDER BY timestamp
            RANGE BETWEEN INTERVAL '5 minutes' PRECEDING AND CURRENT ROW
        ) AS tx_in_last_5min
    FROM transactions
    QUALIFY tx_in_last_5min >= 5
    ORDER BY cardholder_id, timestamp;
    """ |> justParseStatement_ "Velocity (Sliding Window)"


impossibleTravel : Test
impossibleTravel =
    """
    WITH ordered_tx AS (
        SELECT
            cardholder_id,
            timestamp,
            location,
            LAG(timestamp) OVER (PARTITION BY cardholder_id ORDER BY timestamp) AS prev_ts,
            LAG(location)  OVER (PARTITION BY cardholder_id ORDER BY timestamp) AS prev_loc
        FROM transactions
    )
    SELECT
        cardholder_id,
        prev_ts  AS first_tx,
        timestamp AS second_tx,
        prev_loc  AS first_location,
        location  AS second_location,
        EXTRACT(EPOCH FROM (timestamp - prev_ts)) / 60 AS minutes_apart,
        haversine(prev_loc, location)                  AS miles_apart
    FROM ordered_tx
    WHERE prev_ts IS NOT NULL
        AND prev_loc <> location
        AND haversine(prev_loc, location)
                / nullif(EXTRACT(EPOCH FROM (timestamp - prev_ts)), 0)
                * 3600 > 600;
    """ |> justParseStatement_ "Impossible travel"


amountAnomalies : Test
amountAnomalies =
    """
    SELECT cardholder_id, timestamp, amount, merchant_id
    FROM transactions
    WHERE
        (amount >= 99.50  AND amount < 100.00)
        OR (amount >= 499.50 AND amount < 500.00)
        OR amount IN (1.00, 5.00, 10.00)
    ORDER BY cardholder_id, timestamp;
    """ |> justParseStatement_ "Amount anomalies"


suspiciousMerchants : Test
suspiciousMerchants =
    """
    SELECT
        merchant_id,
        date_trunc('hour', timestamp) AS hour_bucket,
        count(DISTINCT cardholder_id) AS unique_cards,
        count(*) AS total_tx,
        sum(amount) AS total_amount
    FROM transactions
    WHERE timestamp >= current_date - INTERVAL '7 days'
    GROUP BY 1, 2
    HAVING count(DISTINCT cardholder_id) > 20
        AND sum(amount) > 5000
    ORDER BY total_amount DESC;
    """ |> justParseStatement_ "Suspicious merchants"


suspiciousMerchantsSelf : Test
suspiciousMerchantsSelf =
    """
    WITH merchant_hourly AS (
        SELECT
            merchant_id,
            date_trunc('hour', timestamp) AS hour_bucket,
            count(DISTINCT cardholder_id) AS unique_cards
        FROM transactions
        WHERE timestamp >= current_date - INTERVAL '60 days'
        GROUP BY 1, 2
        ),
    with_baseline AS (
        SELECT
            *,
            avg(unique_cards) OVER (
            PARTITION BY merchant_id
            ORDER BY hour_bucket
            ROWS BETWEEN 168 PRECEDING AND 1 PRECEDING
            ) AS rolling_avg_cards
        FROM merchant_hourly
    )
    SELECT *,
        unique_cards / nullif(rolling_avg_cards, 0) AS spike_ratio
    FROM with_baseline
    WHERE unique_cards > rolling_avg_cards * 3
    ORDER BY spike_ratio DESC;
    """ |> justParseStatement_ "Suspicious merchant (self compare)"


offHours : Test
offHours =
    """
    WITH cardholder_hour_pattern AS (
        SELECT
            cardholder_id,
            EXTRACT(HOUR FROM timestamp) AS hour_of_day,
            count(*) AS tx_count
        FROM transactions
        WHERE timestamp >= current_date - INTERVAL '90 days'
        GROUP BY 1, 2
        ),
    cardholder_normal AS (
        SELECT
            cardholder_id,
            min(hour_of_day) FILTER (WHERE tx_count >= 2) AS earliest_hour,
            max(hour_of_day) FILTER (WHERE tx_count >= 2) AS latest_hour
        FROM cardholder_hour_pattern
        GROUP BY 1
    )
    SELECT t.cardholder_id, t.timestamp, t.amount, t.merchant_id
    FROM transactions t
    JOIN cardholder_normal cn USING (cardholder_id)
    WHERE EXTRACT(HOUR FROM t.timestamp) NOT BETWEEN cn.earliest_hour AND cn.latest_hour
    ORDER BY t.timestamp DESC;
    """ |> justParseStatement_ "Off hours"


windowFunctions : Test
windowFunctions =
    """
    SELECT
        cardholder_id,
        timestamp,
        amount,
        merchant_id,

        LAG(timestamp) OVER w AS time_of_last,

        CASE WHEN merchant_id <> LAG(merchant_id) OVER w
            THEN 'changed' ELSE 'same' END AS merchant_change,

        sum(amount) OVER (
            PARTITION BY cardholder_id
            ORDER BY timestamp
            RANGE BETWEEN INTERVAL '24 hours' PRECEDING AND CURRENT ROW
        ) AS running_24h_total,

        ROW_NUMBER() OVER (
            PARTITION BY cardholder_id, date(timestamp)
            ORDER BY timestamp
        ) AS tx_of_day

    FROM transactions
    WINDOW w AS (PARTITION BY cardholder_id ORDER BY timestamp)
    ORDER BY cardholder_id, timestamp;
    """ |> justParseStatement_ "Window functions"


windowExample : Test
windowExample =
    """
    SELECT *
    FROM tx_with_windows
    WHERE tx_of_day >= 5
        AND date(time_of_last, '+60 second') < timestamp
        AND merchant_change = 'changed';
    """ |> justParseStatement_ "Window example" |> Test.only
