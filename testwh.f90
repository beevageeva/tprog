
PROGRAM prac2
  INTEGER , dimension(7) :: arr = (/2,5,7,10,18,13,9/)
  INTEGER :: minv

      minv = MINVAL(arr, arr>30)

    print*, minv

END PROGRAM prac2
