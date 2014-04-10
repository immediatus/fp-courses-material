package com.epam.fpp

trait BaseTypeClass[A, B] {
  def self: A
  implicit def F: B
}