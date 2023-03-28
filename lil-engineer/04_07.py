##import required libraries
import pyspark.sql


##create spark session
spark = pyspark.sql.SparkSession \
    .builder \
    .appName("Python Spark SQL basic example") \
    .config('spark.driver.extraClassPath', "/Users/harshittyagi/Downloads/postgresql-42.2.18.jar") \
    .getOrCreate()

##read movies table from db using spark
movies_df = spark.read \
    .format("jdbc") \
    .option("url", "jdbc:postgresql://localhost:5432/etl_pipeline") \
    .option("dbtable", "movies") \
    .option("user", "<username>") \
    .option("password", "<password>") \
    .option("driver", "org.postgresql.Driver") \
    .load()

##read users table from db using spark
users_df = spark.read \
    .format("jdbc") \
    .option("url", "jdbc:postgresql://localhost:5432/etl_pipeline") \
    .option("dbtable", "users") \
    .option("user", "<username>") \
    .option("password", "<password>") \
    .option("driver", "org.postgresql.Driver") \
    .load()


# Use groupBy and mean to aggregate the column
avg_rating = users_df.groupBy('movie_id').mean('rating')

# Join the tables using the film_id column
df = movies_df.join(
    avg_rating,
    movies_df.id==avg_rating.movie_id
)


##print the final dataframe
print(df.show())





