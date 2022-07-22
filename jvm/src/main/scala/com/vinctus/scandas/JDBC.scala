package com.vinctus.scandas

import java.sql.{Connection, DriverManager, ResultSet}
import scala.collection.mutable.ArrayBuffer
import io.github.edadma.table.TextTable

object JDBC:
  def connect(host: String, database: String, user: String, password: String, port: Int): Connection =
    Class.forName("org.postgresql.Driver")
    DriverManager.getConnection(s"jdbc:postgresql://$host:$port/$database", user, password)

  def datasetFromResultSet(rs: ResultSet): Dataset =
    val metadata = rs.getMetaData
    val cols = metadata.getColumnCount
    val columns = 1 to cols map metadata.getColumnName
    val data = new ArrayBuffer[Seq[Any]]

    while (rs.next)
      data += 1 to cols map rs.getObject

    Dataset(columns, data)

  def query(sql: String, conn: Connection): Dataset =
    val stmt = conn.createStatement
    val ds = datasetFromResultSet(stmt.executeQuery(sql))

    stmt.close()
    ds

  def query(sql: String, host: String, database: String, user: String, password: String, port: Int): Dataset =
    val conn = connect(host, database, user, password, port)
    val res = query(sql, conn)

    conn.close()
    res
