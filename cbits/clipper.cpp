/*******************************************************************************
*                                                                              *
* Author    :  Angus Johnson                                                   *
* Version   :  4.3.0                                                           *
* Date      :  16 June 2011                                                    *
* Website   :  http://www.angusj.com                                           *
* Copyright :  Angus Johnson 2010-2011                                         *
*                                                                              *
* License:                                                                     *
* Use, modification & distribution is subject to Boost Software License Ver 1. *
* http://www.boost.org/LICENSE_1_0.txt                                         *
*                                                                              *
* Attributions:                                                                *
* The code in this library is an extension of Bala Vatti's clipping algorithm: *
* "A generic solution to polygon clipping"                                     *
* Communications of the ACM, Vol 35, Issue 7 (July 1992) pp 56-63.             *
* http://portal.acm.org/citation.cfm?id=129906                                 *
*                                                                              *
* Computer graphics and geometric modeling: implementation and algorithms      *
* By Max K. Agoston                                                            *
* Springer; 1 edition (January 4, 2005)                                        *
* http://books.google.com/books?q=vatti+clipping+agoston                       *
*                                                                              *
*******************************************************************************/

/*******************************************************************************
*                                                                              *
* This is a translation of the Delphi Clipper library and the naming style     *
* used has retained a Delphi flavour.                                          *
*                                                                              *
*******************************************************************************/

#include "clipper.hpp"
#include <cmath>
#include <vector>
#include <algorithm>
#include <stdexcept>
#include <cstring>
#include <cstdlib>
#include <stdio.h>
#include <string>
#include <sstream>
#include <iostream>
#include <array>

//Workaround for older compilers that don't have std::abs
#if (__GNUC__ == 2 && __GNUC_MINOR__ <= 97) || (defined(_MSC_VER) && _MSC_VER <= 1310)
namespace std
{
    long long abs(long long x) { return x < 0 ? -x : x; }
}
#endif

static double const horizontal = -3.4E+38;
static double const pi = 3.14159265359;
enum Direction { dRightToLeft, dLeftToRight };

//------------------------------------------------------------------------------
// Int128 class (enables safe math on signed 64bit integers)
// eg Int128 val1((long64)9223372036854775807); //ie 2^63 -1
//    Int128 val2((long64)9223372036854775807);
//    Int128 val3 = val1 * val2;
//    val3.AsString => "85070591730234615847396907784232501249" (8.5e+37)
//------------------------------------------------------------------------------

class Int128
{
  public:

    Int128(long64 _lo = 0)
    {
      hi = 0;
      lo = std::abs(_lo);
      if (_lo < 0) Negate(*this);
    }

    Int128(const Int128 &val): hi(val.hi), lo(val.lo){}

    long64 operator = (const long64 &val)
    {
      hi = 0;
      lo = std::abs(val);
      if (val < 0) Negate(*this);
      return val;
    }

    bool operator == (const Int128 &val) const
      {return (hi == val.hi && lo == val.lo);}

    bool operator != (const Int128 &val) const { return !(*this == val);}

    bool operator > (const Int128 &val) const
    {
      if (hi > val.hi) return true;
      else if (hi < val.hi) return false;
      else if (hi >= 0) return ulong64(lo) > ulong64(val.lo);
      else return ulong64(lo) < ulong64(val.lo);
    }

    bool operator < (const Int128 &val) const
    {
      if (hi < val.hi) return true;
      else if (hi > val.hi) return false;
      else if (hi >= 0) return ulong64(lo) < ulong64(val.lo);
      else return ulong64(lo) > ulong64(val.lo);
    }

    Int128& operator += (const Int128 &rhs)
    {
      long64 xlo = lo;
      hi += rhs.hi; lo += rhs.lo;
      if((xlo < 0 && rhs.lo < 0) || (((xlo < 0) != (rhs.lo < 0)) && lo >= 0))
        hi++;
      return *this;
    }

    Int128 operator + (const Int128 &rhs) const
    {
      Int128 result(*this);
      result+= rhs;
      return result;
    }

    Int128& operator -= (const Int128 &rhs)
    {
      Int128 tmp(rhs);
      Negate(tmp);
      *this += tmp;
      return *this;
    }

    Int128 operator - (const Int128 &rhs) const
    {
      Int128 result(*this);
      result-= rhs;
      return result;
    }

    Int128 operator * (const Int128 &rhs) const {
      if ( !(hi == 0 || hi == -1) || !(rhs.hi == 0 || rhs.hi == -1))
        throw "Int128 operator*: overflow error";
      bool negate = (hi < 0) != (rhs.hi < 0);

      Int128 tmp(*this);
      if (tmp.hi < 0) Negate(tmp);
      ulong64 int1Hi = ulong64(tmp.lo) >> 32;
      ulong64 int1Lo = tmp.lo & 0xFFFFFFFF;

      tmp = rhs;
      if (tmp.hi < 0) Negate(tmp);
      ulong64 int2Hi = ulong64(tmp.lo) >> 32;
      ulong64 int2Lo = tmp.lo & 0xFFFFFFFF;

      //nb: see comments in clipper.pas
      ulong64 a = int1Hi * int2Hi;
      ulong64 b = int1Lo * int2Lo;
      ulong64 c = int1Hi * int2Lo + int1Lo * int2Hi; //nb avoid karatsuba

      tmp.lo = c << 32;
      tmp.hi = a + (c >> 32);
      bool hiBitSet = (tmp.lo < 0);
      tmp.lo += long64(b);
      if ((hiBitSet && (long64(b) < 0)) ||
        ((hiBitSet != (long64(b) < 0)) && (tmp.lo >= 0))) tmp.hi++;

      if (negate) Negate(tmp);
      return tmp;
    }

    Int128 operator/ (const Int128 &rhs) const
    {
      if (rhs.lo == 0 && rhs.hi == 0)
        throw "Int128 operator/: divide by zero";
      bool negate = (rhs.hi < 0) != (hi < 0);
      Int128 result(*this), denom(rhs);
      if (result.hi < 0) Negate(result);
      if (denom.hi < 0)  Negate(denom);
      if (denom > result) return Int128(0); //result is only a fraction of 1
      Negate(denom);

      Int128 p(0);
      for (int i = 0; i < 128; ++i)
      {
        p.hi = p.hi << 1;
        if (p.lo < 0) p.hi++;
        p.lo = long64(p.lo) << 1;
        if (result.hi < 0) p.lo++;
        result.hi = result.hi << 1;
        if (result.lo < 0) result.hi++;
        result.lo = long64(result.lo) << 1;
        Int128 p2(p);
        p += denom;
        if (p.hi < 0) p = p2;
        else result.lo++;
      }
      if (negate) Negate(result);
      return result;
    }

    double AsDouble() const
    {
      const double shift64 = 18446744073709551616.0; //2^64
      if (hi < 0)
      {
        Int128 tmp(*this);
        Negate(tmp);
        return -((double)tmp.lo + (double)tmp.hi * shift64);
      }
      else return (double)lo + (double)hi * shift64;
    }

    //for bug testing ...
    std::string AsString() const
    {
      std::string result;
      int r = 0;
      Int128 tmp(0), val(*this);
      if (hi < 0) Negate(val);
      result.resize(50);
      std::string::size_type i = result.size() -1;
      while (val.hi != 0 || val.lo != 0)
      {
        Div10(val, tmp, r);
        result[i--] = ('0' + r);
        val = tmp;
      }
      if (hi < 0) result[i--] = '-';
      result.erase(0,i+1);
      if (result.size() == 0) result = "0";
      return result;
    }

private:
    long64 hi;
    long64 lo;

    static void Negate(Int128 &val)
    {
      if (val.lo == 0)
      {
        if( val.hi == 0) return;
        val.lo = ~val.lo;
        val.hi = ~val.hi +1;
      }
      else
      {
        val.lo = ~val.lo +1;
        val.hi = ~val.hi;
      }
    }

    //debugging only ...
    void Div10(const Int128 val, Int128& result, int& remainder) const
    {
      remainder = 0;
      result = 0;
      for (int i = 63; i >= 0; --i)
      {
        if ((val.hi & ((long64)1 << i)) != 0)
          remainder = (remainder * 2) + 1; else
          remainder *= 2;
        if (remainder >= 10)
        {
          result.hi += ((long64)1 << i);
          remainder -= 10;
        }
      }
      for (int i = 63; i >= 0; --i)
      {
        if ((val.lo & ((long64)1 << i)) != 0)
          remainder = (remainder * 2) + 1; else
          remainder *= 2;
        if (remainder >= 10)
        {
          result.lo += ((long64)1 << i);
          remainder -= 10;
        }
      }
    }
};

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

bool IsClockwise(const Polygon &poly, bool UseFullInt64Range)
{
  int highI = poly.size() -1;
  if (highI < 2) return false;
  if (UseFullInt64Range)
  {
    Int128 area;
    area = Int128(poly[highI].X) * Int128(poly[0].Y) -
      Int128(poly[0].X) * Int128(poly[highI].Y);
    for (int i = 0; i < highI; ++i)
      area += Int128(poly[i].X) * Int128(poly[i+1].Y) -
        Int128(poly[i+1].X) * Int128(poly[i].Y);
    return area > 0;
  }
  else
  {
    double a;
    a = (double)(poly[highI].X) * (double)(poly[0].Y) -
      (double)(poly[0].X) * (double)(poly[highI].Y);
    for (int i = 0; i < highI; ++i)
      a += (double)(poly[i].X) * (double)(poly[i+1].Y) -
        (double)(poly[i+1].X) * (double)(poly[i].Y);
    return a > 0;
  }
}
//------------------------------------------------------------------------------

bool IsClockwise(OutRec *outRec, bool UseFullInt64Range)
{
  OutPt* startPt = outRec->pts;
  OutPt* op = outRec->pts;
  if (UseFullInt64Range)
  {
    Int128 area(0);
    do
    {
      area += (Int128(op->pt.X) * Int128(op->next->pt.Y)) -
        (Int128(op->next->pt.X) * Int128(op->pt.Y));
      op = op->next;
    }
    while (op != startPt);
    return area > 0;
  }
  else
  {
    double a = 0;
    do
    {
      a += (double)(op->pt.X) * (double)(op->next->pt.Y) -
        (double)(op->next->pt.X) * (double)(op->pt.Y);
      op = op->next;
    }
    while (op != startPt);
    return a > 0;
  }
}
//------------------------------------------------------------------------------

int PointCount(OutPt *pts)
{
  if (!pts) return 0;
  int result = 0;
  OutPt *p = pts;
  do
  {
    result++;
    p = p->next;
  }
  while (p != pts);
  return result;
}
//------------------------------------------------------------------------------

inline bool PointsEqual( const IntPoint &pt1, const IntPoint &pt2)
{
  return ( pt1.X == pt2.X && pt1.Y == pt2.Y );
}
//------------------------------------------------------------------------------

double Area(const Polygon &poly, bool UseFullInt64Range)
{
  int highI = poly.size() -1;
  if (highI < 2) return 0;
  if (UseFullInt64Range) {
    Int128 a(0);
    a = (Int128(poly[highI].X) * Int128(poly[0].Y)) -
      Int128(poly[0].X) * Int128(poly[highI].Y);
    for (int i = 0; i < highI; ++i)
      a += Int128(poly[i].X) * Int128(poly[i+1].Y) -
        Int128(poly[i+1].X) * Int128(poly[i].Y);
    return a.AsDouble() / 2;
  }
  else
  {
    double a;
    a = (double)(poly[highI].X) * (double)(poly[0].Y) -
      (double)(poly[0].X) * (double)(poly[highI].Y);
    for (int i = 0; i < highI; ++i)
      a += (double)(poly[i].X) * (double)(poly[i+1].Y) -
        (double)(poly[i+1].X) * (double)(poly[i].Y);
    return a/2;
  }
}
//------------------------------------------------------------------------------

bool PointIsVertex(const IntPoint &pt, OutPt *pp)
{
  OutPt *pp2 = pp;
  do
  {
    if (PointsEqual(pp2->pt, pt)) return true;
    pp2 = pp2->next;
  }
  while (pp2 != pp);
  return false;
}
//------------------------------------------------------------------------------

bool PointInPolygon(const IntPoint &pt, OutPt *pp, bool UseFullInt64Range)
{
  OutPt *pp2 = pp;
  bool result = false;
  if (UseFullInt64Range) {
    do
    {
      if ((((pp2->pt.Y <= pt.Y) && (pt.Y < pp2->prev->pt.Y)) ||
          ((pp2->prev->pt.Y <= pt.Y) && (pt.Y < pp2->pt.Y))) &&
          Int128(pt.X - pp2->pt.X) < (Int128(pp2->prev->pt.X - pp2->pt.X) *
          Int128(pt.Y - pp2->pt.Y)) / Int128(pp2->prev->pt.Y - pp2->pt.Y))
            result = !result;
      pp2 = pp2->next;
    }
    while (pp2 != pp);
  }
  else
  {
    do
    {
      if ((((pp2->pt.Y <= pt.Y) && (pt.Y < pp2->prev->pt.Y)) ||
        ((pp2->prev->pt.Y <= pt.Y) && (pt.Y < pp2->pt.Y))) &&
        (pt.X - pp2->pt.X < (pp2->prev->pt.X - pp2->pt.X) * (pt.Y - pp2->pt.Y) /
        (pp2->prev->pt.Y - pp2->pt.Y))) result = !result;
      pp2 = pp2->next;
    }
    while (pp2 != pp);
  }
  return result;
}
//------------------------------------------------------------------------------

bool SlopesEqual(TEdge &e1, TEdge &e2, bool UseFullInt64Range)
{
  if (e1.ybot == e1.ytop) return (e2.ybot == e2.ytop);
  else if (e1.xbot == e1.xtop) return (e2.xbot == e2.xtop);
  else if (UseFullInt64Range)
    return Int128(e1.ytop - e1.ybot) * Int128(e2.xtop - e2.xbot) ==
      Int128(e1.xtop - e1.xbot) * Int128(e2.ytop - e2.ybot);
  else return (e1.ytop - e1.ybot)*(e2.xtop - e2.xbot) ==
      (e1.xtop - e1.xbot)*(e2.ytop - e2.ybot);
}
//------------------------------------------------------------------------------

bool SlopesEqual(const IntPoint pt1, const IntPoint pt2,
  const IntPoint pt3, bool UseFullInt64Range)
{
  if (pt1.Y == pt2.Y) return (pt2.Y == pt3.Y);
  else if (pt1.X == pt2.X) return (pt2.X == pt3.X);
  else if (UseFullInt64Range)
    return Int128(pt1.Y-pt2.Y) * Int128(pt2.X-pt3.X) ==
      Int128(pt1.X-pt2.X) * Int128(pt2.Y-pt3.Y);
  else return (pt1.Y-pt2.Y)*(pt2.X-pt3.X) == (pt1.X-pt2.X)*(pt2.Y-pt3.Y);
}
//------------------------------------------------------------------------------

bool SlopesEqual(const IntPoint pt1, const IntPoint pt2,
  const IntPoint pt3, const IntPoint pt4, bool UseFullInt64Range)
{
  if (pt1.Y == pt2.Y) return (pt3.Y == pt4.Y);
  else if (pt1.X == pt2.X) return (pt3.X == pt4.X);
  else if (UseFullInt64Range)
    return Int128(pt1.Y-pt2.Y) * Int128(pt3.X-pt4.X) ==
      Int128(pt1.X-pt2.X) * Int128(pt3.Y-pt4.Y);
  else return (pt1.Y-pt2.Y)*(pt3.X-pt4.X) == (pt1.X-pt2.X)*(pt3.Y-pt4.Y);
}
//------------------------------------------------------------------------------

double GetDx(const IntPoint pt1, const IntPoint pt2)
{
  if (pt1.Y == pt2.Y) return horizontal;
  else return
    (double)(pt2.X - pt1.X) / (double)(pt2.Y - pt1.Y);
}
//---------------------------------------------------------------------------

void SetDx(TEdge &e)
{
  if (e.ybot == e.ytop) e.dx = horizontal;
  else e.dx =
    (double)(e.xtop - e.xbot) / (double)(e.ytop - e.ybot);
}
//---------------------------------------------------------------------------

void SwapSides(TEdge &edge1, TEdge &edge2)
{
  EdgeSide side =  edge1.side;
  edge1.side = edge2.side;
  edge2.side = side;
}
//------------------------------------------------------------------------------

void SwapPolyIndexes(TEdge &edge1, TEdge &edge2)
{
  int outIdx =  edge1.outIdx;
  edge1.outIdx = edge2.outIdx;
  edge2.outIdx = outIdx;
}
//------------------------------------------------------------------------------

inline long64 Round(double val)
{
  if ((val < 0)) return static_cast<long64>(val - 0.5);
  else return static_cast<long64>(val + 0.5);
}
//------------------------------------------------------------------------------

inline long64 Abs(long64 val)
{
  if ((val < 0)) return -val; else return val;
}
//------------------------------------------------------------------------------

long64 TopX(TEdge &edge, const long64 currentY)
{
  if( currentY == edge.ytop ) return edge.xtop;
  return edge.xbot + Round(edge.dx *(currentY - edge.ybot));
}
//------------------------------------------------------------------------------

long64 TopX(const IntPoint pt1, const IntPoint pt2, const long64 currentY)
{
  //preconditions: pt1.Y <> pt2.Y and pt1.Y > pt2.Y
  if (currentY >= pt1.Y) return pt1.X;
  else if (currentY == pt2.Y) return pt2.X;
  else if (pt1.X == pt2.X) return pt1.X;
  else
  {
    double q = (double)(pt1.X-pt2.X)/(double)(pt1.Y-pt2.Y);
    return static_cast<long64>(pt1.X + (currentY - pt1.Y) *q);
  }
}
//------------------------------------------------------------------------------

bool IntersectPoint(TEdge &edge1, TEdge &edge2,
  IntPoint &ip, bool UseFullInt64Range)
{
  double b1, b2;
  if (SlopesEqual(edge1, edge2, UseFullInt64Range)) return false;
  else if (edge1.dx == 0)
  {
    ip.X = edge1.xbot;
    if (edge2.dx == horizontal)
    {
      ip.Y = edge2.ybot;
    } else
    {
      b2 = edge2.ybot - (edge2.xbot/edge2.dx);
      ip.Y = Round(ip.X/edge2.dx + b2);
    }
  }
  else if (edge2.dx == 0)
  {
    ip.X = edge2.xbot;
    if (edge1.dx == horizontal)
    {
      ip.Y = edge1.ybot;
    } else
    {
      b1 = edge1.ybot - (edge1.xbot/edge1.dx);
      ip.Y = Round(ip.X/edge1.dx + b1);
    }
  } else
  {
    b1 = edge1.xbot - edge1.ybot * edge1.dx;
    b2 = edge2.xbot - edge2.ybot * edge2.dx;
    b2 = (b2-b1)/(edge1.dx - edge2.dx);
    ip.Y = Round(b2);
    ip.X = Round(edge1.dx * b2 + b1);
  }

  return
    //can be *so close* to the top of one edge that the rounded Y equals one ytop ...
    (ip.Y == edge1.ytop && ip.Y >= edge2.ytop && edge1.tmpX > edge2.tmpX) ||
    (ip.Y == edge2.ytop && ip.Y >= edge1.ytop && edge1.tmpX > edge2.tmpX) ||
    (ip.Y > edge1.ytop && ip.Y > edge2.ytop);
}
//------------------------------------------------------------------------------

void ReversePolyPtLinks(OutPt &pp)
{
  OutPt *pp1, *pp2;
  pp1 = &pp;
  do {
  pp2 = pp1->next;
  pp1->next = pp1->prev;
  pp1->prev = pp2;
  pp1 = pp2;
  } while( pp1 != &pp );
}
//------------------------------------------------------------------------------

void DisposeOutPts(OutPt*& pp)
{
  if (pp == 0) return;
  pp->prev->next = 0;
  while( pp )
  {
    OutPt *tmpPp = pp;
    pp = pp->next;
    delete tmpPp ;
  }
}
//------------------------------------------------------------------------------

OutRec* CreateOutRec()
{
  OutRec* result = new OutRec;
  result->idx = -1;
  result->isHole = false;
  result->FirstLeft = 0;
  result->AppendLink = 0;
  result->pts = 0;
  return result;
}
//------------------------------------------------------------------------------

void InitEdge(TEdge *e, TEdge *eNext,
  TEdge *ePrev, const IntPoint &pt, PolyType polyType)
{
  std::memset( e, 0, sizeof( TEdge ));

  e->next = eNext;
  e->prev = ePrev;
  e->xcurr = pt.X;
  e->ycurr = pt.Y;
  if (e->ycurr >= e->next->ycurr)
  {
    e->xbot = e->xcurr;
    e->ybot = e->ycurr;
    e->xtop = e->next->xcurr;
    e->ytop = e->next->ycurr;
    e->windDelta = 1;
  } else
  {
    e->xtop = e->xcurr;
    e->ytop = e->ycurr;
    e->xbot = e->next->xcurr;
    e->ybot = e->next->ycurr;
    e->windDelta = -1;
  }
  SetDx(*e);
  e->polyType = polyType;
  e->outIdx = -1;
}
//------------------------------------------------------------------------------

inline void SwapX(TEdge &e)
{
  //swap horizontal edges' top and bottom x's so they follow the natural
  //progression of the bounds - ie so their xbots will align with the
  //adjoining lower edge. [Helpful in the ProcessHorizontal() method.]
  e.xcurr = e.xtop;
  e.xtop = e.xbot;
  e.xbot = e.xcurr;
}
//------------------------------------------------------------------------------

void SwapPoints(IntPoint &pt1, IntPoint &pt2)
{
  IntPoint tmp = pt1;
  pt1 = pt2;
  pt2 = tmp;
}
//------------------------------------------------------------------------------

bool GetOverlapSegment(IntPoint pt1a, IntPoint pt1b, IntPoint pt2a,
  IntPoint pt2b, IntPoint &pt1, IntPoint &pt2)
{
  //precondition: segments are colinear.
  if ( pt1a.Y == pt1b.Y || Abs((pt1a.X - pt1b.X)/(pt1a.Y - pt1b.Y)) > 1 )
  {
    if (pt1a.X > pt1b.X) SwapPoints(pt1a, pt1b);
    if (pt2a.X > pt2b.X) SwapPoints(pt2a, pt2b);
    if (pt1a.X > pt2a.X) pt1 = pt1a; else pt1 = pt2a;
    if (pt1b.X < pt2b.X) pt2 = pt1b; else pt2 = pt2b;
    return pt1.X < pt2.X;
  } else
  {
    if (pt1a.Y < pt1b.Y) SwapPoints(pt1a, pt1b);
    if (pt2a.Y < pt2b.Y) SwapPoints(pt2a, pt2b);
    if (pt1a.Y < pt2a.Y) pt1 = pt1a; else pt1 = pt2a;
    if (pt1b.Y > pt2b.Y) pt2 = pt1b; else pt2 = pt2b;
    return pt1.Y > pt2.Y;
  }
}
//------------------------------------------------------------------------------

OutPt* PolygonBottom(OutPt* pp)
{
  OutPt* p = pp->next;
  OutPt* result = pp;
  while (p != pp)
  {
    if (p->pt.Y > result->pt.Y) result = p;
    else if (p->pt.Y == result->pt.Y && p->pt.X < result->pt.X) result = p;
    p = p->next;
  }
  return result;
}
//------------------------------------------------------------------------------

bool FindSegment(OutPt* &pp, IntPoint &pt1, IntPoint &pt2)
{
  //outPt1 & outPt2 => the overlap segment (if the function returns true)
  if (!pp) return false;
  OutPt* pp2 = pp;
  IntPoint pt1a = pt1, pt2a = pt2;
  do
  {
    if (SlopesEqual(pt1a, pt2a, pp->pt, pp->prev->pt, true) &&
      SlopesEqual(pt1a, pt2a, pp->pt, true) &&
      GetOverlapSegment(pt1a, pt2a, pp->pt, pp->prev->pt, pt1, pt2))
        return true;
    pp = pp->next;
  }
  while (pp != pp2);
  return false;
}
//------------------------------------------------------------------------------

bool Pt3IsBetweenPt1AndPt2(const IntPoint pt1,
  const IntPoint pt2, const IntPoint pt3)
{
  if (PointsEqual(pt1, pt3) || PointsEqual(pt2, pt3)) return true;
  else if (pt1.X != pt2.X) return (pt1.X < pt3.X) == (pt3.X < pt2.X);
  else return (pt1.Y < pt3.Y) == (pt3.Y < pt2.Y);
}
//------------------------------------------------------------------------------

OutPt* InsertPolyPtBetween(OutPt* p1, OutPt* p2, const IntPoint pt)
{
  if (p1 == p2) throw "JoinError";
  OutPt* result = new OutPt;
  result->pt = pt;
  if (p2 == p1->next)
  {
    p1->next = result;
    p2->prev = result;
    result->next = p2;
    result->prev = p1;
  } else
  {
    p2->next = result;
    p1->prev = result;
    result->next = p1;
    result->prev = p2;
  }
  return result;
}

//------------------------------------------------------------------------------
// ClipperBase class methods ...
//------------------------------------------------------------------------------

ClipperBase::ClipperBase() //constructor
{
  m_MinimaList = 0;
  m_CurrentLM = 0;
  m_UseFullRange = true;
}
//------------------------------------------------------------------------------

ClipperBase::~ClipperBase() //destructor
{
  Clear();
}
//------------------------------------------------------------------------------

void ClipperBase::UseFullCoordinateRange(bool newVal)
{
  if (m_edges.size() > 0 && newVal == true)
    throw clipperException("UseFullCoordinateRange() can't be changed "
      "until the Clipper object has been cleared.");
  m_UseFullRange = newVal;
};
//------------------------------------------------------------------------------

bool ClipperBase::AddPolygon( const Polygon &pg, PolyType polyType)
{
  int len = pg.size();
  if (len < 3) return false;
  Polygon p(len);
  p[0] = pg[0];
  int j = 0;
  const long64 MaxVal = 1500000000; //~ Sqrt(2^63)/2

  for (int i = 1; i < len; ++i)
  {
    if (!m_UseFullRange && (Abs(pg[i].X) > MaxVal || Abs(pg[i].Y) > MaxVal))
      throw clipperException("Integer exceeds range bounds");
    else if (PointsEqual(p[j], pg[i])) continue;
    else if (j > 0 && SlopesEqual(p[j-1], p[j], pg[i], m_UseFullRange))
    {
      if (PointsEqual(p[j-1], pg[i])) j--;
    } else j++;
    p[j] = pg[i];
  }
  if (j < 2) return false;

  len = j+1;
  for (;;)
  {
    //nb: test for point equality before testing slopes ...
    if (PointsEqual(p[j], p[0])) j--;
    else if (PointsEqual(p[0], p[1]) ||
      SlopesEqual(p[j], p[0], p[1], m_UseFullRange))
      p[0] = p[j--];
    else if (SlopesEqual(p[j-1], p[j], p[0], m_UseFullRange)) j--;
    else if (SlopesEqual(p[0], p[1], p[2], m_UseFullRange))
    {
      for (int i = 2; i <= j; ++i) p[i-1] = p[i];
      j--;
    }
    //exit loop if nothing is changed or there are too few vertices ...
    if (j == len-1 || j < 2) break;
    len = j +1;
  }
  if (len < 3) return false;

  //create a new edge array ...
  TEdge *edges = new TEdge [len];
  m_edges.push_back(edges);

  //convert vertices to a double-linked-list of edges and initialize ...
  edges[0].xcurr = p[0].X;
  edges[0].ycurr = p[0].Y;
  InitEdge(&edges[len-1], &edges[0], &edges[len-2], p[len-1], polyType);
  for (int i = len-2; i > 0; --i)
    InitEdge(&edges[i], &edges[i+1], &edges[i-1], p[i], polyType);
  InitEdge(&edges[0], &edges[1], &edges[len-1], p[0], polyType);

  //reset xcurr & ycurr and find 'eHighest' (given the Y axis coordinates
  //increase downward so the 'highest' edge will have the smallest ytop) ...
  TEdge *e = &edges[0];
  TEdge *eHighest = e;
  do
  {
    e->xcurr = e->xbot;
    e->ycurr = e->ybot;
    if (e->ytop < eHighest->ytop) eHighest = e;
    e = e->next;
  }
  while ( e != &edges[0]);

  //make sure eHighest is positioned so the following loop works safely ...
  if (eHighest->windDelta > 0) eHighest = eHighest->next;
  if (eHighest->dx == horizontal) eHighest = eHighest->next;

  //finally insert each local minima ...
  e = eHighest;
  do {
    e = AddBoundsToLML(e);
  }
  while( e != eHighest );
  return true;
}
//------------------------------------------------------------------------------

void ClipperBase::InsertLocalMinima(LocalMinima *newLm)
{
  if( ! m_MinimaList )
  {
    m_MinimaList = newLm;
  }
  else if( newLm->Y >= m_MinimaList->Y )
  {
    newLm->next = m_MinimaList;
    m_MinimaList = newLm;
  } else
  {
    LocalMinima* tmpLm = m_MinimaList;
    while( tmpLm->next  && ( newLm->Y < tmpLm->next->Y ) )
      tmpLm = tmpLm->next;
    newLm->next = tmpLm->next;
    tmpLm->next = newLm;
  }
}
//------------------------------------------------------------------------------

TEdge* ClipperBase::AddBoundsToLML(TEdge *e)
{
  //Starting at the top of one bound we progress to the bottom where there's
  //a local minima. We then go to the top of the next bound. These two bounds
  //form the left and right (or right and left) bounds of the local minima.
  e->nextInLML = 0;
  e = e->next;
  for (;;)
  {
    if ( e->dx == horizontal )
    {
      //nb: proceed through horizontals when approaching from their right,
      //    but break on horizontal minima if approaching from their left.
      //    This ensures 'local minima' are always on the left of horizontals.
      if (e->next->ytop < e->ytop && e->next->xbot > e->prev->xbot) break;
      if (e->xtop != e->prev->xbot) SwapX(*e);
      e->nextInLML = e->prev;
    }
    else if (e->ycurr == e->prev->ycurr) break;
    else e->nextInLML = e->prev;
    e = e->next;
  }

  //e and e.prev are now at a local minima ...
  LocalMinima* newLm = new LocalMinima;
  newLm->next = 0;
  newLm->Y = e->prev->ybot;

  if ( e->dx == horizontal ) //horizontal edges never start a left bound
  {
    if (e->xbot != e->prev->xbot) SwapX(*e);
    newLm->leftBound = e->prev;
    newLm->rightBound = e;
  } else if (e->dx < e->prev->dx)
  {
    newLm->leftBound = e->prev;
    newLm->rightBound = e;
  } else
  {
    newLm->leftBound = e;
    newLm->rightBound = e->prev;
  }
  newLm->leftBound->side = esLeft;
  newLm->rightBound->side = esRight;
  InsertLocalMinima( newLm );

  for (;;)
  {
    if ( e->next->ytop == e->ytop && e->next->dx != horizontal ) break;
    e->nextInLML = e->next;
    e = e->next;
    if ( e->dx == horizontal && e->xbot != e->prev->xtop) SwapX(*e);
  }
  return e->next;
}
//------------------------------------------------------------------------------

bool ClipperBase::AddPolygons(const Polygons &ppg, PolyType polyType)
{
  bool result = false;
  for (Polygons::size_type i = 0; i < ppg.size(); ++i)
    if (AddPolygon(ppg[i], polyType)) result = true;
  return result;
}
//------------------------------------------------------------------------------

void ClipperBase::Clear()
{
  DisposeLocalMinimaList();
  for (EdgeList::size_type i = 0; i < m_edges.size(); ++i) delete [] m_edges[i];
  m_edges.clear();
}
//------------------------------------------------------------------------------

void ClipperBase::Reset()
{
  m_CurrentLM = m_MinimaList;
  if( !m_CurrentLM ) return; //ie nothing to process

  //reset all edges ...
  LocalMinima* lm = m_MinimaList;
  while( lm )
  {
    TEdge* e = lm->leftBound;
    while( e )
    {
      e->xcurr = e->xbot;
      e->ycurr = e->ybot;
      e->side = esLeft;
      e->outIdx = -1;
      e = e->nextInLML;
    }
    e = lm->rightBound;
    while( e )
    {
      e->xcurr = e->xbot;
      e->ycurr = e->ybot;
      e->side = esRight;
      e->outIdx = -1;
      e = e->nextInLML;
    }
    lm = lm->next;
  }
}
//------------------------------------------------------------------------------

void ClipperBase::DisposeLocalMinimaList()
{
  while( m_MinimaList )
  {
    LocalMinima* tmpLm = m_MinimaList->next;
    delete m_MinimaList;
    m_MinimaList = tmpLm;
  }
  m_CurrentLM = 0;
}
//------------------------------------------------------------------------------

void ClipperBase::PopLocalMinima()
{
  if( ! m_CurrentLM ) return;
  m_CurrentLM = m_CurrentLM->next;
}
//------------------------------------------------------------------------------

IntRect ClipperBase::GetBounds()
{
  IntRect result;
  LocalMinima* lm = m_MinimaList;
  if (!lm)
  {
    result.left = result.top = result.right = result.bottom = 0;
    return result;
  }
  result.left = lm->leftBound->xbot;
  result.top = lm->leftBound->ybot;
  result.right = lm->leftBound->xbot;
  result.bottom = lm->leftBound->ybot;
  while (lm)
  {
    if (lm->leftBound->ybot > result.bottom)
      result.bottom = lm->leftBound->ybot;
    TEdge* e = lm->leftBound;
    for (;;) {
      while (e->nextInLML)
      {
        if (e->xbot < result.left) result.left = e->xbot;
        if (e->xbot > result.right) result.right = e->xbot;
        e = e->nextInLML;
      }
      if (e->xbot < result.left) result.left = e->xbot;
      if (e->xbot > result.right) result.right = e->xbot;
      if (e->xtop < result.left) result.left = e->xtop;
      if (e->xtop > result.right) result.right = e->xtop;
      if (e->ytop < result.top) result.top = e->ytop;

      if (e == lm->leftBound) e = lm->rightBound;
      else break;
    }
    lm = lm->next;
  }
  return result;
}


//------------------------------------------------------------------------------
// TClipper methods ...
//------------------------------------------------------------------------------

Clipper::Clipper() : ClipperBase() //constructor
{
  m_Scanbeam = 0;
  m_ActiveEdges = 0;
  m_SortedEdges = 0;
  m_IntersectNodes = 0;
  m_ExecuteLocked = false;
};
//------------------------------------------------------------------------------

Clipper::~Clipper() //destructor
{
  Clear();
  DisposeScanbeamList();
};
//------------------------------------------------------------------------------

void Clipper::Clear()
{
  if (m_edges.size() == 0) return; //avoids problems with ClipperBase destructor
  DisposeAllPolyPts();
  ClipperBase::Clear();
};
//------------------------------------------------------------------------------

void Clipper::DisposeScanbeamList()
{
  while ( m_Scanbeam ) {
  Scanbeam* sb2 = m_Scanbeam->next;
  delete m_Scanbeam;
  m_Scanbeam = sb2;
  }
}
//------------------------------------------------------------------------------

void Clipper::Reset()
{
  ClipperBase::Reset();
  m_Scanbeam = 0;
  m_ActiveEdges = 0;
  m_SortedEdges = 0;
  LocalMinima* lm = m_MinimaList;
  while (lm)
  {
    InsertScanbeam(lm->Y);
    InsertScanbeam(lm->leftBound->ytop);
    lm = lm->next;
  }
}
//------------------------------------------------------------------------------

bool Clipper::Execute(ClipType clipType, Polygons &solution,
    PolyFillType subjFillType, PolyFillType clipFillType)
{
  if( m_ExecuteLocked ) return false;
  m_ExecuteLocked = true;
  solution.resize(0);
  m_SubjFillType = subjFillType;
  m_ClipFillType = clipFillType;
  m_ClipType = clipType;
  bool succeeded = ExecuteInternal(false);
  if (succeeded) BuildResult(solution);
  m_ExecuteLocked = false;
  return succeeded;
}
//------------------------------------------------------------------------------

bool Clipper::Execute(ClipType clipType, ExPolygons &solution,
    PolyFillType subjFillType, PolyFillType clipFillType)
{
  if( m_ExecuteLocked ) return false;
  m_ExecuteLocked = true;
  solution.resize(0);
  m_SubjFillType = subjFillType;
  m_ClipFillType = clipFillType;
  m_ClipType = clipType;
  bool succeeded = ExecuteInternal(true);
  if (succeeded) BuildResultEx(solution);
  m_ExecuteLocked = false;
  return succeeded;
}
//------------------------------------------------------------------------------

bool PolySort(OutRec *or1, OutRec *or2)
{
  if (or1 == or2) return false;
  if (!or1->pts || !or2->pts)
  {
    if (or1->pts != or2->pts)
    {
      if (or1->pts) return true; else return false;
    }
    else return false;
  }
  int i1, i2;
  if (or1->isHole)
    i1 = or1->FirstLeft->idx; else
    i1 = or1->idx;
  if (or2->isHole)
    i2 = or2->FirstLeft->idx; else
    i2 = or2->idx;
  int result = i1 - i2;
  if (result == 0 && (or1->isHole != or2->isHole))
  {
    if (or1->isHole) return false;
    else return true;
  }
  else return result < 0;
}
//------------------------------------------------------------------------------

OutRec* FindAppendLinkEnd(OutRec *outRec)
{
  while (outRec->AppendLink) outRec = outRec->AppendLink;
  return outRec;
}
//------------------------------------------------------------------------------

void Clipper::FixHoleLinkage(OutRec *outRec)
{
  OutRec *tmp;
  if (outRec->bottomPt)
    tmp = m_PolyOuts[outRec->bottomPt->idx]->FirstLeft; else
    tmp = outRec->FirstLeft;
  //avoid a very rare endless loop (via recursion) ...
  if (outRec == tmp)
  {
    outRec->FirstLeft = 0;
    outRec->AppendLink = 0;
    outRec->isHole = false;
    return;
  }

  if (tmp)
  {
    if (tmp->AppendLink) tmp = FindAppendLinkEnd(tmp);
    if (tmp == outRec) tmp = 0;
    else if (tmp->isHole)
    {
      FixHoleLinkage(tmp);
      tmp = tmp->FirstLeft;
    }
  }
  outRec->FirstLeft = tmp;
  if (!tmp) outRec->isHole = false;
  outRec->AppendLink = 0;
}
//------------------------------------------------------------------------------

bool Clipper::ExecuteInternal(bool fixHoleLinkages)
{
  bool succeeded;
  try {
    Reset();
    if (!m_CurrentLM ) return true;
    long64 botY = PopScanbeam();
    do {
      InsertLocalMinimaIntoAEL(botY);
      ClearHorzJoins();
      ProcessHorizontals();
      long64 topY = PopScanbeam();
      succeeded = ProcessIntersections(topY);
      if (!succeeded) break;
      ProcessEdgesAtTopOfScanbeam(topY);
      botY = topY;
    } while( m_Scanbeam );
  }
  catch(...) {
    succeeded = false;
  }

  if (succeeded)
  {
    //tidy up output polygons and fix orientations where necessary ...
    for (PolyOutList::size_type i = 0; i < m_PolyOuts.size(); ++i)
    {
      OutRec *outRec = m_PolyOuts[i];
      if (!outRec->pts) continue;
      FixupOutPolygon(*outRec);
      if (!outRec->pts) continue;
      if (outRec->isHole && fixHoleLinkages) FixHoleLinkage(outRec);
      if (outRec->isHole == IsClockwise(outRec, m_UseFullRange))
        ReversePolyPtLinks(*outRec->pts);
    }

    JoinCommonEdges();
    if (fixHoleLinkages)
      std::sort(m_PolyOuts.begin(), m_PolyOuts.end(), PolySort);
  }

  ClearJoins();
  ClearHorzJoins();
  return succeeded;
}
//------------------------------------------------------------------------------

void Clipper::InsertScanbeam(const long64 Y)
{
  if( !m_Scanbeam )
  {
    m_Scanbeam = new Scanbeam;
    m_Scanbeam->next = 0;
    m_Scanbeam->Y = Y;
  }
  else if(  Y > m_Scanbeam->Y )
  {
    Scanbeam* newSb = new Scanbeam;
    newSb->Y = Y;
    newSb->next = m_Scanbeam;
    m_Scanbeam = newSb;
  } else
  {
    Scanbeam* sb2 = m_Scanbeam;
    while( sb2->next  && ( Y <= sb2->next->Y ) ) sb2 = sb2->next;
    if(  Y == sb2->Y ) return; //ie ignores duplicates
    Scanbeam* newSb = new Scanbeam;
    newSb->Y = Y;
    newSb->next = sb2->next;
    sb2->next = newSb;
  }
}
//------------------------------------------------------------------------------

long64 Clipper::PopScanbeam()
{
  long64 Y = m_Scanbeam->Y;
  Scanbeam* sb2 = m_Scanbeam;
  m_Scanbeam = m_Scanbeam->next;
  delete sb2;
  return Y;
}
//------------------------------------------------------------------------------

void Clipper::DisposeAllPolyPts(){
  for (PolyOutList::size_type i = 0; i < m_PolyOuts.size(); ++i)
    DisposeOutRec(i);
  m_PolyOuts.clear();
}
//------------------------------------------------------------------------------

void Clipper::DisposeOutRec(int index, bool ignorePts)
{
  OutRec *outRec = m_PolyOuts[index];
  if (!ignorePts && outRec->pts) DisposeOutPts(outRec->pts);
  delete outRec;
  m_PolyOuts[index] = 0;
}
//------------------------------------------------------------------------------

void Clipper::SetWindingCount(TEdge &edge)
{
  TEdge *e = edge.prevInAEL;
  //find the edge of the same polytype that immediately preceeds 'edge' in AEL
  while ( e  && e->polyType != edge.polyType ) e = e->prevInAEL;
  if ( !e )
  {
    edge.windCnt = edge.windDelta;
    edge.windCnt2 = 0;
    e = m_ActiveEdges; //ie get ready to calc windCnt2
  } else if ( IsNonZeroFillType(edge) )
  {
    //nonZero filling ...
    if ( e->windCnt * e->windDelta < 0 )
    {
      if (Abs(e->windCnt) > 1)
      {
        if (e->windDelta * edge.windDelta < 0) edge.windCnt = e->windCnt;
        else edge.windCnt = e->windCnt + edge.windDelta;
      } else
        edge.windCnt = e->windCnt + e->windDelta + edge.windDelta;
    } else
    {
      if ( Abs(e->windCnt) > 1 && e->windDelta * edge.windDelta < 0)
        edge.windCnt = e->windCnt;
      else if ( e->windCnt + edge.windDelta == 0 )
        edge.windCnt = e->windCnt;
      else edge.windCnt = e->windCnt + edge.windDelta;
    }
    edge.windCnt2 = e->windCnt2;
    e = e->nextInAEL; //ie get ready to calc windCnt2
  } else
  {
    //even-odd filling ...
    edge.windCnt = 1;
    edge.windCnt2 = e->windCnt2;
    e = e->nextInAEL; //ie get ready to calc windCnt2
  }

  //update windCnt2 ...
  if ( IsNonZeroAltFillType(edge) )
  {
    //nonZero filling ...
    while ( e != &edge )
    {
      edge.windCnt2 += e->windDelta;
      e = e->nextInAEL;
    }
  } else
  {
    //even-odd filling ...
    while ( e != &edge )
    {
      edge.windCnt2 = (edge.windCnt2 == 0) ? 1 : 0;
      e = e->nextInAEL;
    }
  }
}
//------------------------------------------------------------------------------

bool Clipper::IsNonZeroFillType(const TEdge& edge) const
{
  if (edge.polyType == ptSubject)
    return m_SubjFillType == pftNonZero; else
    return m_ClipFillType == pftNonZero;
}
//------------------------------------------------------------------------------

bool Clipper::IsNonZeroAltFillType(const TEdge& edge) const
{
  if (edge.polyType == ptSubject)
    return m_ClipFillType == pftNonZero; else
    return m_SubjFillType == pftNonZero;
}
//------------------------------------------------------------------------------

bool Clipper::IsContributing(const TEdge& edge) const
{
  switch( m_ClipType ){
    case ctIntersection:
      if ( edge.polyType == ptSubject )
        return Abs(edge.windCnt) == 1 && edge.windCnt2 != 0; else
        return Abs(edge.windCnt2) > 0 && Abs(edge.windCnt) == 1;
    case ctUnion:
      return Abs(edge.windCnt) == 1 && edge.windCnt2 == 0;
    case ctDifference:
      if ( edge.polyType == ptSubject )
        return std::abs(edge.windCnt) == 1 && edge.windCnt2 == 0; else
        return std::abs(edge.windCnt) == 1 && edge.windCnt2 != 0;
    default: //case ctXor:
      return std::abs(edge.windCnt) == 1;
  }
}
//------------------------------------------------------------------------------

void Clipper::AddLocalMinPoly(TEdge *e1, TEdge *e2, const IntPoint &pt)
{
  if( e2->dx == horizontal || ( e1->dx > e2->dx ) )
  {
    AddOutPt( e1, pt );
    e2->outIdx = e1->outIdx;
    e1->side = esLeft;
    e2->side = esRight;
  } else
  {
    AddOutPt( e2, pt );
    e1->outIdx = e2->outIdx;
    e1->side = esRight;
    e2->side = esLeft;
  }
}
//------------------------------------------------------------------------------

void Clipper::AddLocalMaxPoly(TEdge *e1, TEdge *e2, const IntPoint &pt)
{
  AddOutPt( e1, pt );
  if( e1->outIdx == e2->outIdx )
  {
    e1->outIdx = -1;
    e2->outIdx = -1;
  }
  else
    AppendPolygon( e1, e2 );
}
//------------------------------------------------------------------------------

void Clipper::AddEdgeToSEL(TEdge *edge)
{
  //SEL pointers in PEdge are reused to build a list of horizontal edges.
  //However, we don't need to worry about order with horizontal edge processing.
  if( !m_SortedEdges )
  {
    m_SortedEdges = edge;
    edge->prevInSEL = 0;
    edge->nextInSEL = 0;
  }
  else
  {
    edge->nextInSEL = m_SortedEdges;
    edge->prevInSEL = 0;
    m_SortedEdges->prevInSEL = edge;
    m_SortedEdges = edge;
  }
}
//------------------------------------------------------------------------------

void Clipper::CopyAELToSEL()
{
  TEdge* e = m_ActiveEdges;
  m_SortedEdges = e;
  if (!m_ActiveEdges) return;
  m_SortedEdges->prevInSEL = 0;
  e = e->nextInAEL;
  while ( e )
  {
    e->prevInSEL = e->prevInAEL;
    e->prevInSEL->nextInSEL = e;
    e->nextInSEL = 0;
    e = e->nextInAEL;
  }
}
//------------------------------------------------------------------------------

void Clipper::AddJoin(TEdge *e1, TEdge *e2, int e1OutIdx, int e2OutIdx)
{
  JoinRec* jr = new JoinRec;
  if (e1OutIdx >= 0)
    jr->poly1Idx = e1OutIdx; else
    jr->poly1Idx = e1->outIdx;
  jr->pt1a = IntPoint(e1->xcurr, e1->ycurr);
  jr->pt1b = IntPoint(e1->xtop, e1->ytop);
  if (e2OutIdx >= 0)
    jr->poly2Idx = e2OutIdx; else
    jr->poly2Idx = e2->outIdx;
  jr->pt2a = IntPoint(e2->xcurr, e2->ycurr);
  jr->pt2b = IntPoint(e2->xtop, e2->ytop);
  m_Joins.push_back(jr);
}
//------------------------------------------------------------------------------

void Clipper::ClearJoins()
{
  for (JoinList::size_type i = 0; i < m_Joins.size(); i++)
    delete m_Joins[i];
  m_Joins.resize(0);
}
//------------------------------------------------------------------------------

void Clipper::AddHorzJoin(TEdge *e, int idx)
{
  HorzJoinRec* hj = new HorzJoinRec;
  hj->edge = e;
  hj->savedIdx = idx;
  m_HorizJoins.push_back(hj);
}
//------------------------------------------------------------------------------

void Clipper::ClearHorzJoins()
{
  for (HorzJoinList::size_type i = 0; i < m_HorizJoins.size(); i++)
    delete m_HorizJoins[i];
  m_HorizJoins.resize(0);
}
//------------------------------------------------------------------------------

void Clipper::InsertLocalMinimaIntoAEL( const long64 botY)
{
  while(  m_CurrentLM  && ( m_CurrentLM->Y == botY ) )
  {
    TEdge* lb = m_CurrentLM->leftBound;
    TEdge* rb = m_CurrentLM->rightBound;

    InsertEdgeIntoAEL( lb );
    InsertScanbeam( lb->ytop );
    InsertEdgeIntoAEL( rb );

    if ( IsNonZeroFillType( *lb) )
      rb->windDelta = -lb->windDelta;
    else
    {
      lb->windDelta = 1;
      rb->windDelta = 1;
    }
    SetWindingCount( *lb );
    rb->windCnt = lb->windCnt;
    rb->windCnt2 = lb->windCnt2;

    if(  rb->dx == horizontal )
    {
      //nb: only rightbounds can have a horizontal bottom edge
      AddEdgeToSEL( rb );
      InsertScanbeam( rb->nextInLML->ytop );
    }
    else
      InsertScanbeam( rb->ytop );

    if( IsContributing(*lb) )
      AddLocalMinPoly( lb, rb, IntPoint(lb->xcurr, m_CurrentLM->Y) );

    //if output polygons share an edge, they'll need joining later ...
    if (lb->outIdx >= 0 && lb->prevInAEL &&
      lb->prevInAEL->outIdx >= 0 && lb->prevInAEL->xcurr == lb->xbot &&
       SlopesEqual(*lb, *lb->prevInAEL, m_UseFullRange))
         AddJoin(lb, lb->prevInAEL);

    //if any output polygons share an edge, they'll need joining later ...
    if (rb->outIdx >= 0)
    {
      if (rb->dx == horizontal)
      {
        for (HorzJoinList::size_type i = 0; i < m_HorizJoins.size(); ++i)
        {
          IntPoint pt, pt2; //returned by GetOverlapSegment() but unused here.
          HorzJoinRec* hj = m_HorizJoins[i];
          //if horizontals rb and hj.edge overlap, flag for joining later ...
          if (GetOverlapSegment(IntPoint(hj->edge->xbot, hj->edge->ybot),
            IntPoint(hj->edge->xtop, hj->edge->ytop),
            IntPoint(rb->xbot, rb->ybot),
            IntPoint(rb->xtop, rb->ytop), pt, pt2))
              AddJoin(hj->edge, rb, hj->savedIdx);
        }
      }
    }

    if( lb->nextInAEL != rb )
    {
      if (rb->outIdx >= 0 && rb->prevInAEL->outIdx >= 0 &&
        SlopesEqual(*rb->prevInAEL, *rb, m_UseFullRange))
          AddJoin(rb, rb->prevInAEL);

      TEdge* e = lb->nextInAEL;
      IntPoint pt = IntPoint(lb->xcurr, lb->ycurr);
      while( e != rb )
      {
        if(!e) throw clipperException("InsertLocalMinimaIntoAEL: missing rightbound!");
        //nb: For calculating winding counts etc, IntersectEdges() assumes
        //that param1 will be to the right of param2 ABOVE the intersection ...
        IntersectEdges( rb , e , pt , ipNone); //order important here
        e = e->nextInAEL;
      }
    }
    PopLocalMinima();
  }
}
//------------------------------------------------------------------------------

void Clipper::DeleteFromAEL(TEdge *e)
{
  TEdge* AelPrev = e->prevInAEL;
  TEdge* AelNext = e->nextInAEL;
  if(  !AelPrev &&  !AelNext && (e != m_ActiveEdges) ) return; //already deleted
  if( AelPrev ) AelPrev->nextInAEL = AelNext;
  else m_ActiveEdges = AelNext;
  if( AelNext ) AelNext->prevInAEL = AelPrev;
  e->nextInAEL = 0;
  e->prevInAEL = 0;
}
//------------------------------------------------------------------------------

void Clipper::DeleteFromSEL(TEdge *e)
{
  TEdge* SelPrev = e->prevInSEL;
  TEdge* SelNext = e->nextInSEL;
  if(  !SelPrev &&  !SelNext && (e != m_SortedEdges) ) return; //already deleted
  if( SelPrev ) SelPrev->nextInSEL = SelNext;
  else m_SortedEdges = SelNext;
  if( SelNext ) SelNext->prevInSEL = SelPrev;
  e->nextInSEL = 0;
  e->prevInSEL = 0;
}
//------------------------------------------------------------------------------

void Clipper::IntersectEdges(TEdge *e1, TEdge *e2,
     const IntPoint &pt, IntersectProtects protects)
{
  //e1 will be to the left of e2 BELOW the intersection. Therefore e1 is before
  //e2 in AEL except when e1 is being inserted at the intersection point ...
  bool e1stops = !(ipLeft & protects) &&  !e1->nextInLML &&
    e1->xtop == pt.X && e1->ytop == pt.Y;
  bool e2stops = !(ipRight & protects) &&  !e2->nextInLML &&
    e2->xtop == pt.X && e2->ytop == pt.Y;
  bool e1Contributing = ( e1->outIdx >= 0 );
  bool e2contributing = ( e2->outIdx >= 0 );

  //update winding counts...
  //assumes that e1 will be to the right of e2 ABOVE the intersection
  if ( e1->polyType == e2->polyType )
  {
    if ( IsNonZeroFillType( *e1) )
    {
      if (e1->windCnt + e2->windDelta == 0 ) e1->windCnt = -e1->windCnt;
      else e1->windCnt += e2->windDelta;
      if ( e2->windCnt - e1->windDelta == 0 ) e2->windCnt = -e2->windCnt;
      else e2->windCnt -= e1->windDelta;
    } else
    {
      int oldE1WindCnt = e1->windCnt;
      e1->windCnt = e2->windCnt;
      e2->windCnt = oldE1WindCnt;
    }
  } else
  {
    if ( IsNonZeroFillType(*e2) ) e1->windCnt2 += e2->windDelta;
    else e1->windCnt2 = ( e1->windCnt2 == 0 ) ? 1 : 0;
    if ( IsNonZeroFillType(*e1) ) e2->windCnt2 -= e1->windDelta;
    else e2->windCnt2 = ( e2->windCnt2 == 0 ) ? 1 : 0;
  }

  if ( e1Contributing && e2contributing )
  {
    if ( e1stops || e2stops || std::abs(e1->windCnt) > 1 ||
      std::abs(e2->windCnt) > 1 ||
      (e1->polyType != e2->polyType && m_ClipType != ctXor) )
        AddLocalMaxPoly(e1, e2, pt); else
        DoBothEdges( e1, e2, pt );
  }
  else if ( e1Contributing )
  {
    switch( m_ClipType ) {
      case ctIntersection:
        if ( (e2->polyType == ptSubject || e2->windCnt2 != 0) &&
           std::abs(e2->windCnt) < 2 ) DoEdge1( e1, e2, pt);
        break;
      default:
        if ( std::abs(e2->windCnt) < 2 ) DoEdge1(e1, e2, pt);
    }
  }
  else if ( e2contributing )
  {
    if ( m_ClipType == ctIntersection )
    {
        if ( (e1->polyType == ptSubject || e1->windCnt2 != 0) &&
          std::abs(e1->windCnt) < 2 ) DoEdge2( e1, e2, pt );
    }
    else
      if (std::abs(e1->windCnt) < 2) DoEdge2( e1, e2, pt );

  } else
  {
    //neither edge is currently contributing ...
    if ( std::abs(e1->windCnt) > 1 && std::abs(e2->windCnt) > 1 ) ;// do nothing
    else if ( e1->polyType != e2->polyType && !e1stops && !e2stops &&
      std::abs(e1->windCnt) < 2 && std::abs(e2->windCnt) < 2 )
        AddLocalMinPoly(e1, e2, pt);
    else if ( std::abs(e1->windCnt) == 1 && std::abs(e2->windCnt) == 1 )
      switch( m_ClipType ) {
        case ctIntersection:
          if ( std::abs(e1->windCnt2) > 0 && std::abs(e2->windCnt2) > 0 )
            AddLocalMinPoly(e1, e2, pt);
          break;
        case ctUnion:
          if ( e1->windCnt2 == 0 && e2->windCnt2 == 0 )
            AddLocalMinPoly(e1, e2, pt);
          break;
        case ctDifference:
          if ( (e1->polyType == ptClip && e2->polyType == ptClip &&
            e1->windCnt2 != 0 && e2->windCnt2 != 0) ||
            (e1->polyType == ptSubject && e2->polyType == ptSubject &&
            e1->windCnt2 == 0 && e2->windCnt2 == 0) )
              AddLocalMinPoly(e1, e2, pt);
          break;
        case ctXor:
          AddLocalMinPoly(e1, e2, pt);
      }
    else if ( std::abs(e1->windCnt) < 2 && std::abs(e2->windCnt) < 2 )
      SwapSides( *e1, *e2 );
  }

  if(  (e1stops != e2stops) &&
    ( (e1stops && (e1->outIdx >= 0)) || (e2stops && (e2->outIdx >= 0)) ) )
  {
    SwapSides( *e1, *e2 );
    SwapPolyIndexes( *e1, *e2 );
  }

  //finally, delete any non-contributing maxima edges  ...
  if( e1stops ) DeleteFromAEL( e1 );
  if( e2stops ) DeleteFromAEL( e2 );
}
//------------------------------------------------------------------------------

void Clipper::SetHoleState(TEdge *e, OutRec *outRec)
{
  bool isHole = false;
  TEdge *e2 = e->prevInAEL;
  while (e2)
  {
    if (e2->outIdx >= 0)
    {
      isHole = !isHole;
      if (! outRec->FirstLeft)
        outRec->FirstLeft = m_PolyOuts[e2->outIdx];
    }
    e2 = e2->prevInAEL;
  }
  if (isHole) outRec->isHole = true;
}
//------------------------------------------------------------------------------

bool GetNextNonDupOutPt(OutPt* pp, OutPt*& next)
{
  next = pp->next;
  while (next != pp && PointsEqual(pp->pt, next->pt))
    next = next->next;
  return next != pp;
}
//------------------------------------------------------------------------------

bool GetPrevNonDupOutPt(OutPt* pp, OutPt*& prev)
{
  prev = pp->prev;
  while (prev != pp && PointsEqual(pp->pt, prev->pt))
    prev = prev->prev;
  return prev != pp;
}
//------------------------------------------------------------------------------

void Clipper::AppendPolygon(TEdge *e1, TEdge *e2)
{
  //get the start and ends of both output polygons ...
  OutRec *outRec1 = m_PolyOuts[e1->outIdx];
  OutRec *outRec2 = m_PolyOuts[e2->outIdx];

  //work out which polygon fragment has the correct hole state ...
  OutPt *bPt1 = outRec1->bottomPt;
  OutPt *bPt2 = outRec2->bottomPt;
  OutRec *holeStateRec;
  OutPt *next1, *next2, *prev1, *prev2;
  if (bPt1->pt.Y > bPt2->pt.Y) holeStateRec = outRec1;
  else if (bPt1->pt.Y < bPt2->pt.Y) holeStateRec = outRec2;
  else if (bPt1->pt.X < bPt2->pt.X) holeStateRec = outRec1;
  else if (bPt1->pt.X > bPt2->pt.X) holeStateRec = outRec2;
  else if (!GetNextNonDupOutPt(bPt1, next1)) holeStateRec = outRec2;
  else if (!GetNextNonDupOutPt(bPt2, next2)) holeStateRec = outRec1;
  else
  {
    GetPrevNonDupOutPt(bPt1, prev1);
    GetPrevNonDupOutPt(bPt2, prev2);
    double dx1 = GetDx(bPt1->pt, next1->pt);
    double dx2 = GetDx(bPt1->pt, prev1->pt);
    if (dx2 > dx1) dx1 = dx2;
    dx2 = GetDx(bPt2->pt, next2->pt);
    if (dx2 > dx1) holeStateRec = outRec2;
    else
    {
      dx2 = GetDx(bPt2->pt, prev2->pt);
      if (dx2 > dx1) holeStateRec = outRec2;
      else holeStateRec = outRec1;
    }
  }

  //fixup hole status ...
  if (outRec1->isHole != outRec2->isHole) {
    if (holeStateRec == outRec2) outRec1->isHole = outRec2->isHole; 
    else outRec2->isHole = outRec1->isHole;
  }

  OutPt* p1_lft = outRec1->pts;
  OutPt* p1_rt = p1_lft->prev;
  OutPt* p2_lft = outRec2->pts;
  OutPt* p2_rt = p2_lft->prev;

  EdgeSide side;
  //join e2 poly onto e1 poly and delete pointers to e2 ...
  if(  e1->side == esLeft )
  {
    if(  e2->side == esLeft )
    {
      //z y x a b c
      ReversePolyPtLinks(*p2_lft);
      p2_lft->next = p1_lft;
      p1_lft->prev = p2_lft;
      p1_rt->next = p2_rt;
      p2_rt->prev = p1_rt;
      outRec1->pts = p2_rt;
    } else
    {
      //x y z a b c
      p2_rt->next = p1_lft;
      p1_lft->prev = p2_rt;
      p2_lft->prev = p1_rt;
      p1_rt->next = p2_lft;
      outRec1->pts = p2_lft;
    }
    side = esLeft;
  } else
  {
    if(  e2->side == esRight )
    {
      //a b c z y x
      ReversePolyPtLinks( *p2_lft );
      p1_rt->next = p2_rt;
      p2_rt->prev = p1_rt;
      p2_lft->next = p1_lft;
      p1_lft->prev = p2_lft;
    } else
    {
      //a b c x y z
      p1_rt->next = p2_lft;
      p2_lft->prev = p1_rt;
      p1_lft->prev = p2_rt;
      p2_rt->next = p1_lft;
    }
    side = esRight;
  }

  if (holeStateRec == outRec2)
    outRec1->bottomPt = outRec2->bottomPt;
  outRec2->pts = 0;
  outRec2->bottomPt = 0;
  outRec2->AppendLink = outRec1;
  int OKIdx = e1->outIdx;
  int ObsoleteIdx = e2->outIdx;

  e1->outIdx = -1; //nb: safe because we only get here via AddLocalMaxPoly
  e2->outIdx = -1;

  TEdge* e = m_ActiveEdges;
  while( e )
  {
    if( e->outIdx == ObsoleteIdx )
    {
      e->outIdx = OKIdx;
      e->side = side;
      break;
    }
    e = e->nextInAEL;
  }

  for (JoinList::size_type i = 0; i < m_Joins.size(); ++i)
  {
      if (m_Joins[i]->poly1Idx == ObsoleteIdx) m_Joins[i]->poly1Idx = OKIdx;
      if (m_Joins[i]->poly2Idx == ObsoleteIdx) m_Joins[i]->poly2Idx = OKIdx;
  }

  for (HorzJoinList::size_type i = 0; i < m_HorizJoins.size(); ++i)
  {
      if (m_HorizJoins[i]->savedIdx == ObsoleteIdx)
        m_HorizJoins[i]->savedIdx = OKIdx;
  }

}
//------------------------------------------------------------------------------

void Clipper::AddOutPt(TEdge *e, const IntPoint &pt)
{
  bool ToFront = (e->side == esLeft);
  if(  e->outIdx < 0 )
  {
    OutRec *outRec = CreateOutRec();
    m_PolyOuts.push_back(outRec);
    outRec->idx = m_PolyOuts.size()-1;
    e->outIdx = outRec->idx;
    OutPt* op = new OutPt;
    outRec->pts = op;
    outRec->bottomPt = op;
    op->pt = pt;
    op->idx = outRec->idx;
    op->next = op;
    op->prev = op;
    SetHoleState(e, outRec);
  } else
  {
    OutRec *outRec = m_PolyOuts[e->outIdx];
    OutPt* op = outRec->pts;
    if ((ToFront && PointsEqual(pt, op->pt)) ||
      (!ToFront && PointsEqual(pt, op->prev->pt))) return;
    OutPt* op2 = new OutPt;
    op2->pt = pt;
    op2->idx = outRec->idx;
    if (op2->pt.Y == outRec->bottomPt->pt.Y &&
      op2->pt.X < outRec->bottomPt->pt.X) outRec->bottomPt = op2;
    op2->next = op;
    op2->prev = op->prev;
    op2->prev->next = op2;
    op->prev = op2;
    if (ToFront) outRec->pts = op2;
  }
}
//------------------------------------------------------------------------------

void Clipper::ProcessHorizontals()
{
  TEdge* horzEdge = m_SortedEdges;
  while( horzEdge )
  {
    DeleteFromSEL( horzEdge );
    ProcessHorizontal( horzEdge );
    horzEdge = m_SortedEdges;
  }
}
//------------------------------------------------------------------------------

bool Clipper::IsTopHorz(const long64 XPos)
{
  TEdge* e = m_SortedEdges;
  while( e )
  {
    if(  ( XPos >= std::min(e->xcurr, e->xtop) ) &&
      ( XPos <= std::max(e->xcurr, e->xtop) ) ) return false;
    e = e->nextInSEL;
  }
  return true;
}
//------------------------------------------------------------------------------

bool IsMinima(TEdge *e)
{
  return e  && (e->prev->nextInLML != e) && (e->next->nextInLML != e);
}
//------------------------------------------------------------------------------

bool IsMaxima(TEdge *e, const long64 Y)
{
  return e && e->ytop == Y && !e->nextInLML;
}
//------------------------------------------------------------------------------

bool IsIntermediate(TEdge *e, const long64 Y)
{
  return e->ytop == Y && e->nextInLML;
}
//------------------------------------------------------------------------------

TEdge *GetMaximaPair(TEdge *e)
{
  if( !IsMaxima(e->next, e->ytop) || e->next->xtop != e->xtop )
    return e->prev; else
    return e->next;
}
//------------------------------------------------------------------------------

void Clipper::SwapPositionsInAEL(TEdge *edge1, TEdge *edge2)
{
  if(  !edge1->nextInAEL &&  !edge1->prevInAEL ) return;
  if(  !edge2->nextInAEL &&  !edge2->prevInAEL ) return;

  if(  edge1->nextInAEL == edge2 )
  {
    TEdge* next = edge2->nextInAEL;
    if( next ) next->prevInAEL = edge1;
    TEdge* prev = edge1->prevInAEL;
    if( prev ) prev->nextInAEL = edge2;
    edge2->prevInAEL = prev;
    edge2->nextInAEL = edge1;
    edge1->prevInAEL = edge2;
    edge1->nextInAEL = next;
  }
  else if(  edge2->nextInAEL == edge1 )
  {
    TEdge* next = edge1->nextInAEL;
    if( next ) next->prevInAEL = edge2;
    TEdge* prev = edge2->prevInAEL;
    if( prev ) prev->nextInAEL = edge1;
    edge1->prevInAEL = prev;
    edge1->nextInAEL = edge2;
    edge2->prevInAEL = edge1;
    edge2->nextInAEL = next;
  }
  else
  {
    TEdge* next = edge1->nextInAEL;
    TEdge* prev = edge1->prevInAEL;
    edge1->nextInAEL = edge2->nextInAEL;
    if( edge1->nextInAEL ) edge1->nextInAEL->prevInAEL = edge1;
    edge1->prevInAEL = edge2->prevInAEL;
    if( edge1->prevInAEL ) edge1->prevInAEL->nextInAEL = edge1;
    edge2->nextInAEL = next;
    if( edge2->nextInAEL ) edge2->nextInAEL->prevInAEL = edge2;
    edge2->prevInAEL = prev;
    if( edge2->prevInAEL ) edge2->prevInAEL->nextInAEL = edge2;
  }

  if( !edge1->prevInAEL ) m_ActiveEdges = edge1;
  else if( !edge2->prevInAEL ) m_ActiveEdges = edge2;
}
//------------------------------------------------------------------------------

void Clipper::SwapPositionsInSEL(TEdge *edge1, TEdge *edge2)
{
  if(  !( edge1->nextInSEL ) &&  !( edge1->prevInSEL ) ) return;
  if(  !( edge2->nextInSEL ) &&  !( edge2->prevInSEL ) ) return;

  if(  edge1->nextInSEL == edge2 )
  {
    TEdge* next = edge2->nextInSEL;
    if( next ) next->prevInSEL = edge1;
    TEdge* prev = edge1->prevInSEL;
    if( prev ) prev->nextInSEL = edge2;
    edge2->prevInSEL = prev;
    edge2->nextInSEL = edge1;
    edge1->prevInSEL = edge2;
    edge1->nextInSEL = next;
  }
  else if(  edge2->nextInSEL == edge1 )
  {
    TEdge* next = edge1->nextInSEL;
    if( next ) next->prevInSEL = edge2;
    TEdge* prev = edge2->prevInSEL;
    if( prev ) prev->nextInSEL = edge1;
    edge1->prevInSEL = prev;
    edge1->nextInSEL = edge2;
    edge2->prevInSEL = edge1;
    edge2->nextInSEL = next;
  }
  else
  {
    TEdge* next = edge1->nextInSEL;
    TEdge* prev = edge1->prevInSEL;
    edge1->nextInSEL = edge2->nextInSEL;
    if( edge1->nextInSEL ) edge1->nextInSEL->prevInSEL = edge1;
    edge1->prevInSEL = edge2->prevInSEL;
    if( edge1->prevInSEL ) edge1->prevInSEL->nextInSEL = edge1;
    edge2->nextInSEL = next;
    if( edge2->nextInSEL ) edge2->nextInSEL->prevInSEL = edge2;
    edge2->prevInSEL = prev;
    if( edge2->prevInSEL ) edge2->prevInSEL->nextInSEL = edge2;
  }

  if( !edge1->prevInSEL ) m_SortedEdges = edge1;
  else if( !edge2->prevInSEL ) m_SortedEdges = edge2;
}
//------------------------------------------------------------------------------

TEdge* GetNextInAEL(TEdge *e, Direction dir)
{
  if( dir == dLeftToRight ) return e->nextInAEL;
  else return e->prevInAEL;
}
//------------------------------------------------------------------------------

void Clipper::ProcessHorizontal(TEdge *horzEdge)
{
  Direction dir;
  long64 horzLeft, horzRight;

  if( horzEdge->xcurr < horzEdge->xtop )
  {
    horzLeft = horzEdge->xcurr;
    horzRight = horzEdge->xtop;
    dir = dLeftToRight;
  } else
  {
    horzLeft = horzEdge->xtop;
    horzRight = horzEdge->xcurr;
    dir = dRightToLeft;
  }

  TEdge* eMaxPair;
  if( horzEdge->nextInLML ) eMaxPair = 0;
  else eMaxPair = GetMaximaPair(horzEdge);

  TEdge* e = GetNextInAEL( horzEdge , dir );
  while( e )
  {
    TEdge* eNext = GetNextInAEL( e, dir );
    if( e->xcurr >= horzLeft && e->xcurr <= horzRight )
    {
      //ok, so far it looks like we're still in range of the horizontal edge
      if ( e->xcurr == horzEdge->xtop && horzEdge->nextInLML)
      {
        if (SlopesEqual(*e, *horzEdge->nextInLML, m_UseFullRange))
        {
          //if output polygons share an edge, they'll need joining later ...
          if (horzEdge->outIdx >= 0 && e->outIdx >= 0)
            AddJoin(horzEdge->nextInLML, e, horzEdge->outIdx);
          break; //we've reached the end of the horizontal line
        }
        else if (e->dx < horzEdge->nextInLML->dx)
        //we really have got to the end of the intermediate horz edge so quit.
        //nb: More -ve slopes follow more +ve slopes ABOVE the horizontal.
          break;
      }

      if( e == eMaxPair )
      {
        //horzEdge is evidently a maxima horizontal and we've arrived at its end.
        if (dir == dLeftToRight)
          IntersectEdges(horzEdge, e, IntPoint(e->xcurr, horzEdge->ycurr), ipNone);
        else
          IntersectEdges(e, horzEdge, IntPoint(e->xcurr, horzEdge->ycurr), ipNone);
        return;
      }
      else if( e->dx == horizontal &&  !IsMinima(e) && !(e->xcurr > e->xtop) )
      {
        //An overlapping horizontal edge. Overlapping horizontal edges are
        //processed as if layered with the current horizontal edge (horizEdge)
        //being infinitesimally lower that the next (e). Therfore, we
        //intersect with e only if e.xcurr is within the bounds of horzEdge ...
        if( dir == dLeftToRight )
          IntersectEdges( horzEdge , e, IntPoint(e->xcurr, horzEdge->ycurr),
            (IsTopHorz( e->xcurr ))? ipLeft : ipBoth );
        else
          IntersectEdges( e, horzEdge, IntPoint(e->xcurr, horzEdge->ycurr),
            (IsTopHorz( e->xcurr ))? ipRight : ipBoth );
      }
      else if( dir == dLeftToRight )
      {
        IntersectEdges( horzEdge, e, IntPoint(e->xcurr, horzEdge->ycurr),
          (IsTopHorz( e->xcurr ))? ipLeft : ipBoth );
      }
      else
      {
        IntersectEdges( e, horzEdge, IntPoint(e->xcurr, horzEdge->ycurr),
          (IsTopHorz( e->xcurr ))? ipRight : ipBoth );
      }
      SwapPositionsInAEL( horzEdge, e );
    }
    else if( dir == dLeftToRight &&
      e->xcurr > horzRight  && m_SortedEdges ) break;
    else if( dir == dRightToLeft &&
      e->xcurr < horzLeft && m_SortedEdges ) break;
    e = eNext;
  } //end while

  if( horzEdge->nextInLML )
  {
    if( horzEdge->outIdx >= 0 )
      AddOutPt( horzEdge, IntPoint(horzEdge->xtop, horzEdge->ytop));
    UpdateEdgeIntoAEL( horzEdge );
  }
  else
  {
    if ( horzEdge->outIdx >= 0 )
      IntersectEdges( horzEdge, eMaxPair,
      IntPoint(horzEdge->xtop, horzEdge->ycurr), ipBoth);
    if (eMaxPair->outIdx >= 0) throw clipperException("ProcessHorizontal error");
    DeleteFromAEL(eMaxPair);
    DeleteFromAEL(horzEdge);
  }
}
//------------------------------------------------------------------------------

void Clipper::UpdateEdgeIntoAEL(TEdge *&e)
{
  if( !e->nextInLML ) throw
    clipperException("UpdateEdgeIntoAEL: invalid call");
  TEdge* AelPrev = e->prevInAEL;
  TEdge* AelNext = e->nextInAEL;
  e->nextInLML->outIdx = e->outIdx;
  if( AelPrev ) AelPrev->nextInAEL = e->nextInLML;
  else m_ActiveEdges = e->nextInLML;
  if( AelNext ) AelNext->prevInAEL = e->nextInLML;
  e->nextInLML->side = e->side;
  e->nextInLML->windDelta = e->windDelta;
  e->nextInLML->windCnt = e->windCnt;
  e->nextInLML->windCnt2 = e->windCnt2;
  e = e->nextInLML;
  e->prevInAEL = AelPrev;
  e->nextInAEL = AelNext;
  if( e->dx != horizontal ) InsertScanbeam( e->ytop );
}
//------------------------------------------------------------------------------

bool Clipper::ProcessIntersections( const long64 topY)
{
  if( !m_ActiveEdges ) return true;
  try {
    BuildIntersectList(topY);
    if ( !m_IntersectNodes) return true;
    if ( FixupIntersections() ) ProcessIntersectList();
    else return false;
  }
  catch(...) {
    m_SortedEdges = 0;
    DisposeIntersectNodes();
    throw clipperException("ProcessIntersections error");
  }
  return true;
}
//------------------------------------------------------------------------------

void Clipper::DisposeIntersectNodes()
{
  while ( m_IntersectNodes )
  {
    IntersectNode* iNode = m_IntersectNodes->next;
    delete m_IntersectNodes;
    m_IntersectNodes = iNode;
  }
}
//------------------------------------------------------------------------------

void Clipper::BuildIntersectList(const long64 topY)
{
  if ( !m_ActiveEdges ) return;

  //prepare for sorting ...
  TEdge* e = m_ActiveEdges;
  e->tmpX = TopX( *e, topY );
  m_SortedEdges = e;
  m_SortedEdges->prevInSEL = 0;
  e = e->nextInAEL;
  while( e )
  {
    e->prevInSEL = e->prevInAEL;
    e->prevInSEL->nextInSEL = e;
    e->nextInSEL = 0;
    e->tmpX = TopX( *e, topY );
    e = e->nextInAEL;
  }

  //bubblesort ...
  bool isModified = true;
  while( isModified && m_SortedEdges )
  {
    isModified = false;
    e = m_SortedEdges;
    while( e->nextInSEL )
    {
      TEdge *eNext = e->nextInSEL;
      IntPoint pt;
      if(e->tmpX > eNext->tmpX &&
        IntersectPoint(*e, *eNext, pt, m_UseFullRange))
      {
        AddIntersectNode( e, eNext, pt );
        SwapPositionsInSEL(e, eNext);
        isModified = true;
      }
      else
        e = eNext;
    }
    if( e->prevInSEL ) e->prevInSEL->nextInSEL = 0;
    else break;
  }
  m_SortedEdges = 0;
}
//------------------------------------------------------------------------------

bool Process1Before2(IntersectNode &node1, IntersectNode &node2)
{
  bool result;
  if (node1.pt.Y == node2.pt.Y)
  {
    if (node1.edge1 == node2.edge1 || node1.edge2 == node2.edge1)
    {
      result = node2.pt.X > node1.pt.X;
      if (node2.edge1->dx > 0) return !result; else return result;
    }
    else if (node1.edge1 == node2.edge2 || node1.edge2 == node2.edge2)
    {
      result = node2.pt.X > node1.pt.X;
      if (node2.edge2->dx > 0) return !result; else return result;
    }
    else return node2.pt.X > node1.pt.X;
  }
  else return node1.pt.Y > node2.pt.Y;
}
//------------------------------------------------------------------------------

void Clipper::AddIntersectNode(TEdge *e1, TEdge *e2, const IntPoint &pt)
{
  IntersectNode* newNode = new IntersectNode;
  newNode->edge1 = e1;
  newNode->edge2 = e2;
  newNode->pt = pt;
  newNode->next = 0;
  if( !m_IntersectNodes ) m_IntersectNodes = newNode;
  else if(  Process1Before2(*newNode, *m_IntersectNodes) )
  {
    newNode->next = m_IntersectNodes;
    m_IntersectNodes = newNode;
  }
  else
  {
    IntersectNode* iNode = m_IntersectNodes;
    while( iNode->next  && Process1Before2(*iNode->next, *newNode) )
        iNode = iNode->next;
    newNode->next = iNode->next;
    iNode->next = newNode;
  }
}
//------------------------------------------------------------------------------

void Clipper::ProcessIntersectList()
{
  while( m_IntersectNodes )
  {
    IntersectNode* iNode = m_IntersectNodes->next;
    {
      IntersectEdges( m_IntersectNodes->edge1 ,
        m_IntersectNodes->edge2 , m_IntersectNodes->pt, ipBoth );
      SwapPositionsInAEL( m_IntersectNodes->edge1 , m_IntersectNodes->edge2 );
    }
    delete m_IntersectNodes;
    m_IntersectNodes = iNode;
  }
}
//------------------------------------------------------------------------------

void Clipper::DoMaxima(TEdge *e, long64 topY)
{
  TEdge* eMaxPair = GetMaximaPair(e);
  long64 X = e->xtop;
  TEdge* eNext = e->nextInAEL;
  while( eNext != eMaxPair )
  {
    if (!eNext) throw clipperException("DoMaxima error");
    IntersectEdges( e, eNext, IntPoint(X, topY), ipBoth );
    eNext = eNext->nextInAEL;
  }
  if( e->outIdx < 0 && eMaxPair->outIdx < 0 )
  {
    DeleteFromAEL( e );
    DeleteFromAEL( eMaxPair );
  }
  else if( e->outIdx >= 0 && eMaxPair->outIdx >= 0 )
  {
    IntersectEdges( e, eMaxPair, IntPoint(X, topY), ipNone );
  }
  else throw clipperException("DoMaxima error");
}
//------------------------------------------------------------------------------

void Clipper::ProcessEdgesAtTopOfScanbeam(const long64 topY)
{
  TEdge* e = m_ActiveEdges;
  while( e )
  {
    //1. process maxima, treating them as if they're 'bent' horizontal edges,
    //   but exclude maxima with horizontal edges. nb: e can't be a horizontal.
    if( IsMaxima(e, topY) && GetMaximaPair(e)->dx != horizontal )
    {
      //'e' might be removed from AEL, as may any following edges so ...
      TEdge* ePrior = e->prevInAEL;
      DoMaxima(e, topY);
      if( !ePrior ) e = m_ActiveEdges;
      else e = ePrior->nextInAEL;
    }
    else
    {
      //2. promote horizontal edges, otherwise update xcurr and ycurr ...
      if(  IsIntermediate(e, topY) && e->nextInLML->dx == horizontal )
      {
        if (e->outIdx >= 0)
        {
          AddOutPt(e, IntPoint(e->xtop, e->ytop));

          for (HorzJoinList::size_type i = 0; i < m_HorizJoins.size(); ++i)
          {
            IntPoint pt, pt2;
            HorzJoinRec* hj = m_HorizJoins[i];
            if (GetOverlapSegment(IntPoint(hj->edge->xbot, hj->edge->ybot),
              IntPoint(hj->edge->xtop, hj->edge->ytop),
              IntPoint(e->nextInLML->xbot, e->nextInLML->ybot),
              IntPoint(e->nextInLML->xtop, e->nextInLML->ytop), pt, pt2))
                AddJoin(hj->edge, e->nextInLML, hj->savedIdx, e->outIdx);
          }

          AddHorzJoin(e->nextInLML, e->outIdx);
        }
        UpdateEdgeIntoAEL(e);
        AddEdgeToSEL(e);
      } else
      {
        //this just simplifies horizontal processing ...
        e->xcurr = TopX( *e, topY );
        e->ycurr = topY;
      }
      e = e->nextInAEL;
    }
  }

  //3. Process horizontals at the top of the scanbeam ...
  ProcessHorizontals();

  //4. Promote intermediate vertices ...
  e = m_ActiveEdges;
  while( e )
  {
    if( IsIntermediate( e, topY ) )
    {
      if( e->outIdx >= 0 ) AddOutPt(e, IntPoint(e->xtop,e->ytop));
      UpdateEdgeIntoAEL(e);

      //if output polygons share an edge, they'll need joining later ...
      if (e->outIdx >= 0 && e->prevInAEL && e->prevInAEL->outIdx >= 0 &&
        e->prevInAEL->xcurr == e->xbot && e->prevInAEL->ycurr == e->ybot &&
        SlopesEqual(IntPoint(e->xbot,e->ybot), IntPoint(e->xtop, e->ytop),
          IntPoint(e->xbot,e->ybot),
          IntPoint(e->prevInAEL->xtop, e->prevInAEL->ytop), m_UseFullRange))
      {
        AddOutPt(e->prevInAEL, IntPoint(e->xbot, e->ybot));
        AddJoin(e, e->prevInAEL);
      }
      else if (e->outIdx >= 0 && e->nextInAEL && e->nextInAEL->outIdx >= 0 &&
        e->nextInAEL->ycurr > e->nextInAEL->ytop &&
        e->nextInAEL->ycurr < e->nextInAEL->ybot &&
        e->nextInAEL->xcurr == e->xbot && e->nextInAEL->ycurr == e->ybot &&
        SlopesEqual(IntPoint(e->xbot,e->ybot), IntPoint(e->xtop, e->ytop),
          IntPoint(e->xbot,e->ybot),
          IntPoint(e->nextInAEL->xtop, e->nextInAEL->ytop), m_UseFullRange))
      {
        AddOutPt(e->nextInAEL, IntPoint(e->xbot, e->ybot));
        AddJoin(e, e->nextInAEL);
      }
    }
    e = e->nextInAEL;
  }
}
//------------------------------------------------------------------------------

void Clipper::FixupOutPolygon(OutRec &outRec)
{
  //FixupOutPolygon() - removes duplicate points and simplifies consecutive
  //parallel edges by removing the middle vertex.
  OutPt *lastOK = 0;
  outRec.pts = outRec.bottomPt;
  OutPt *pp = outRec.bottomPt;

  for (;;)
  {
    if (pp->prev == pp || pp->prev == pp->next )
    {
      DisposeOutPts(pp);
      outRec.pts = 0;
      outRec.bottomPt = 0;
      return;
    }
    //test for duplicate points and for same slope (cross-product) ...
    if ( PointsEqual(pp->pt, pp->next->pt) ||
      SlopesEqual(pp->prev->pt, pp->pt, pp->next->pt, m_UseFullRange) )
    {
      lastOK = 0;
      OutPt *tmp = pp;
      if (pp == outRec.bottomPt)
      {
          if (tmp->prev->pt.Y > tmp->next->pt.Y)
            outRec.bottomPt = tmp->prev; else
            outRec.bottomPt = tmp->next;
          outRec.pts = outRec.bottomPt;
      }
      pp->prev->next = pp->next;
      pp->next->prev = pp->prev;
      pp = pp->prev;
      delete tmp;
    }
    else if (pp == lastOK) break;
    else
    {
      if (!lastOK) lastOK = pp;
      pp = pp->next;
    }
  }
}
//------------------------------------------------------------------------------

void Clipper::BuildResult(Polygons &polys)
{
  int k = 0;
  polys.resize(m_PolyOuts.size());
  for (PolyOutList::size_type i = 0; i < m_PolyOuts.size(); ++i)
  {
    if (m_PolyOuts[i]->pts)
    {
      Polygon* pg = &polys[k];
      pg->clear();
      OutPt* p = m_PolyOuts[i]->pts;
      do
      {
        pg->push_back(p->pt);
        p = p->next;
      } while (p != m_PolyOuts[i]->pts);
      //make sure each polygon has at least 3 vertices ...
      if (pg->size() < 3) pg->clear(); else k++;
    }
  }
  polys.resize(k);
}
//------------------------------------------------------------------------------

void Clipper::BuildResultEx(ExPolygons &polys)
{
  PolyOutList::size_type i = 0;
  int k = 0;
  polys.resize(0);
  polys.reserve(m_PolyOuts.size());
  while (i < m_PolyOuts.size() && m_PolyOuts[i]->pts)
  {
    ExPolygon epg;
    OutPt* p = m_PolyOuts[i]->pts;
    do {
      epg.outer.push_back(p->pt);
      p = p->next;
    } while (p != m_PolyOuts[i]->pts);
    i++;
    //make sure polygons have at least 3 vertices ...
    if (epg.outer.size() < 3) continue;
    while (i < m_PolyOuts.size()
      && m_PolyOuts[i]->pts && m_PolyOuts[i]->isHole)
    {
      Polygon pg;
      p = m_PolyOuts[i]->pts;
      do {
        pg.push_back(p->pt);
        p = p->next;
      } while (p != m_PolyOuts[i]->pts);
      epg.holes.push_back(pg);
      i++;
    }
    polys.push_back(epg);
    k++;
  }
  polys.resize(k);
}
//------------------------------------------------------------------------------

void SwapIntersectNodes(IntersectNode &int1, IntersectNode &int2)
{
  TEdge *e1 = int1.edge1;
  TEdge *e2 = int1.edge2;
  IntPoint p = int1.pt;

  int1.edge1 = int2.edge1;
  int1.edge2 = int2.edge2;
  int1.pt = int2.pt;

  int2.edge1 = e1;
  int2.edge2 = e2;
  int2.pt = p;
}
//------------------------------------------------------------------------------

bool Clipper::FixupIntersections()
{
  if ( !m_IntersectNodes->next ) return true;

  CopyAELToSEL();
  IntersectNode *int1 = m_IntersectNodes;
  IntersectNode *int2 = m_IntersectNodes->next;
  while (int2)
  {
    TEdge *e1 = int1->edge1;
    TEdge *e2;
    if (e1->prevInSEL == int1->edge2) e2 = e1->prevInSEL;
    else if (e1->nextInSEL == int1->edge2) e2 = e1->nextInSEL;
    else
    {
      //The current intersection is out of order, so try and swap it with
      //a subsequent intersection ...
      while (int2)
      {
        if (int2->edge1->nextInSEL == int2->edge2 ||
          int2->edge1->prevInSEL == int2->edge2) break;
        else int2 = int2->next;
      }
      if ( !int2 ) return false; //oops!!!

      //found an intersect node that can be swapped ...
      SwapIntersectNodes(*int1, *int2);
      e1 = int1->edge1;
      e2 = int1->edge2;
    }
    SwapPositionsInSEL(e1, e2);
    int1 = int1->next;
    int2 = int1->next;
  }

  m_SortedEdges = 0;

  //finally, check the last intersection too ...
  return (int1->edge1->prevInSEL == int1->edge2 ||
    int1->edge1->nextInSEL == int1->edge2);
}
//------------------------------------------------------------------------------

bool E2InsertsBeforeE1(TEdge &e1, TEdge &e2)
{
  if (e2.xcurr == e1.xcurr) return e2.dx > e1.dx;
  else return e2.xcurr < e1.xcurr;
}
//------------------------------------------------------------------------------

void Clipper::InsertEdgeIntoAEL(TEdge *edge)
{
  edge->prevInAEL = 0;
  edge->nextInAEL = 0;
  if( !m_ActiveEdges )
  {
    m_ActiveEdges = edge;
  }
  else if( E2InsertsBeforeE1(*m_ActiveEdges, *edge) )
  {
    edge->nextInAEL = m_ActiveEdges;
    m_ActiveEdges->prevInAEL = edge;
    m_ActiveEdges = edge;
  } else
  {
    TEdge* e = m_ActiveEdges;
    while( e->nextInAEL  && !E2InsertsBeforeE1(*e->nextInAEL , *edge) )
      e = e->nextInAEL;
    edge->nextInAEL = e->nextInAEL;
    if( e->nextInAEL ) e->nextInAEL->prevInAEL = edge;
    edge->prevInAEL = e;
    e->nextInAEL = edge;
  }
}
//----------------------------------------------------------------------

void Clipper::DoEdge1(TEdge *edge1, TEdge *edge2, const IntPoint &pt)
{
  AddOutPt(edge1, pt);
  SwapSides(*edge1, *edge2);
  SwapPolyIndexes(*edge1, *edge2);
}
//----------------------------------------------------------------------

void Clipper::DoEdge2(TEdge *edge1, TEdge *edge2, const IntPoint &pt)
{
  AddOutPt(edge2, pt);
  SwapSides(*edge1, *edge2);
  SwapPolyIndexes(*edge1, *edge2);
}
//----------------------------------------------------------------------

void Clipper::DoBothEdges(TEdge *edge1, TEdge *edge2, const IntPoint &pt)
{
  AddOutPt(edge1, pt);
  AddOutPt(edge2, pt);
  SwapSides( *edge1 , *edge2 );
  SwapPolyIndexes( *edge1 , *edge2 );
}
//----------------------------------------------------------------------

void Clipper::JoinCommonEdges()
{
  for (JoinList::size_type i = 0; i < m_Joins.size(); i++)
  {
    JoinRec* j = m_Joins[i];
    OutRec *outRec1 = m_PolyOuts[j->poly1Idx];
    OutPt *pp1a = outRec1->pts;
    OutRec *outRec2 = m_PolyOuts[j->poly2Idx];
    OutPt *pp2a = outRec2->pts;
    IntPoint pt1 = j->pt2a, pt2 = j->pt2b;
    IntPoint pt3 = j->pt1a, pt4 = j->pt1b;
    if (!FindSegment(pp1a, pt1, pt2)) continue;
    if (j->poly1Idx == j->poly2Idx)
    {
      //we're searching the same polygon for overlapping segments so
      //segment 2 mustn't be the same as segment 1 ...
      pp2a = pp1a->next;
      if (!FindSegment(pp2a, pt3, pt4) || (pp2a == pp1a)) continue;
    }
    else if (!FindSegment(pp2a, pt3, pt4)) continue;

    if (!GetOverlapSegment(pt1, pt2, pt3, pt4, pt1, pt2)) continue;

    OutPt *p1, *p2, *p3, *p4;
    OutPt *prev = pp1a->prev;
    //get p1 & p2 polypts - the overlap start & endpoints on poly1
    if (PointsEqual(pp1a->pt, pt1)) p1 = pp1a;
    else if (PointsEqual(prev->pt, pt1)) p1 = prev;
    else p1 = InsertPolyPtBetween(pp1a, prev, pt1);

    if (PointsEqual(pp1a->pt, pt2)) p2 = pp1a;
    else if (PointsEqual(prev->pt, pt2)) p2 = prev;
    else if ((p1 == pp1a) || (p1 == prev))
      p2 = InsertPolyPtBetween(pp1a, prev, pt2);
    else if (Pt3IsBetweenPt1AndPt2(pp1a->pt, p1->pt, pt2))
      p2 = InsertPolyPtBetween(pp1a, p1, pt2); else
      p2 = InsertPolyPtBetween(p1, prev, pt2);

    //get p3 & p4 polypts - the overlap start & endpoints on poly2
    prev = pp2a->prev;
    if (PointsEqual(pp2a->pt, pt1)) p3 = pp2a;
    else if (PointsEqual(prev->pt, pt1)) p3 = prev;
    else p3 = InsertPolyPtBetween(pp2a, prev, pt1);

    if (PointsEqual(pp2a->pt, pt2)) p4 = pp2a;
    else if (PointsEqual(prev->pt, pt2)) p4 = prev;
    else if ((p3 == pp2a) || (p3 == prev))
      p4 = InsertPolyPtBetween(pp2a, prev, pt2);
    else if (Pt3IsBetweenPt1AndPt2(pp2a->pt, p3->pt, pt2))
      p4 = InsertPolyPtBetween(pp2a, p3, pt2); else
      p4 = InsertPolyPtBetween(p3, prev, pt2);

    //p1.pt == p3.pt and p2.pt == p4.pt so join p1 to p3 and p2 to p4 ...
    if (p1->next == p2 && p3->prev == p4)
    {
      p1->next = p3;
      p3->prev = p1;
      p2->prev = p4;
      p4->next = p2;
    }
    else if (p1->prev == p2 && p3->next == p4)
    {
      p1->prev = p3;
      p3->next = p1;
      p2->next = p4;
      p4->prev = p2;
    }
    else
      continue; //an orientation is probably wrong

    if (j->poly2Idx == j->poly1Idx)
    {
      //instead of joining two polygons, we've just created a new one by
      //splitting one polygon into two.
      //However, make sure the longer (and presumed larger) polygon is attached
      //to outRec1 in case it also owns some holes ...
      if (PointCount(p1) > PointCount(p2))
      {
          outRec1->pts = PolygonBottom(p1);
          outRec1->bottomPt = outRec1->pts;
          outRec2 = CreateOutRec();
          m_PolyOuts.push_back(outRec2);
          outRec2->idx = m_PolyOuts.size()-1;
          j->poly2Idx = outRec2->idx;
          outRec2->pts = PolygonBottom(p2);
          outRec2->bottomPt = outRec2->pts;
      }
      else
      {
          outRec1->pts = PolygonBottom(p2);
          outRec1->bottomPt = outRec1->pts;
          outRec2 = CreateOutRec();
          m_PolyOuts.push_back(outRec2);
          outRec2->idx = m_PolyOuts.size()-1;
          j->poly2Idx = outRec2->idx;
          outRec2->pts = PolygonBottom(p1);
          outRec2->bottomPt = outRec2->pts;
      }

      if (PointInPolygon(outRec2->pts->pt, outRec1->pts, m_UseFullRange))
      {
        outRec2->isHole = !outRec1->isHole;
        outRec2->FirstLeft = outRec1;
        if (outRec2->isHole == IsClockwise(outRec2, m_UseFullRange))
          ReversePolyPtLinks(*outRec2->pts);
      } else if (PointInPolygon(outRec1->pts->pt, outRec2->pts, m_UseFullRange))
      {
        outRec2->isHole = outRec1->isHole;
        outRec1->isHole = !outRec2->isHole;
        outRec2->FirstLeft = outRec1->FirstLeft;
        outRec1->FirstLeft = outRec2;
        if (outRec1->isHole == IsClockwise(outRec1, m_UseFullRange))
          ReversePolyPtLinks(*outRec1->pts);
      } else
      {
        //I'm assuming that if outRec1 contain any holes, it still does after
        //the split and that none are now contained by the new outRec2.
        //In a perfect world, I'd PointInPolygon() every hole owned by outRec1
        //to make sure it's still owned by outRec1 and not now owned by outRec2.
        outRec2->isHole = outRec1->isHole;
        outRec2->FirstLeft = outRec1->FirstLeft;
      }

      //now fixup any subsequent joins that match this polygon
      for (JoinList::size_type k = i+1; k < m_Joins.size(); k++)
      {
        JoinRec* j2 = m_Joins[k];
        if (j2->poly1Idx == j->poly1Idx && PointIsVertex(j2->pt1a, p2))
          j2->poly1Idx = j->poly2Idx;
        if (j2->poly2Idx == j->poly1Idx && PointIsVertex(j2->pt2a, p2))
          j2->poly2Idx = j->poly2Idx;
      }
    } else
    {
      //having joined 2 polygons together, delete the obsolete pointer ...
      outRec2->pts = 0;
      outRec2->bottomPt = 0;
      outRec2->AppendLink = outRec1;

      //now fixup any subsequent Joins that match this polygon
      for (JoinList::size_type k = i+1; k < m_Joins.size(); k++)
      {
        JoinRec* j2 = m_Joins[k];
        if (j2->poly1Idx == j->poly2Idx) j2->poly1Idx = j->poly1Idx;
        if (j2->poly2Idx == j->poly2Idx) j2->poly2Idx = j->poly1Idx;
      }
      j->poly2Idx = j->poly1Idx;
    }

    //now cleanup redundant edges too ...
    FixupOutPolygon(*outRec1);
    if (j->poly2Idx != j->poly1Idx) FixupOutPolygon(*outRec2);
  }
}

//------------------------------------------------------------------------------
// OffsetPolygon functions ...
//------------------------------------------------------------------------------

struct DoublePoint
{
  double X;
  double Y;
  DoublePoint(double x = 0, double y = 0) : X(x), Y(y) {}
};

Polygon BuildArc(const IntPoint &pt,
  const double a1, const double a2, const double r)
{
  int steps = std::max(6, int(std::sqrt(std::abs(r)) * std::abs(a2 - a1)));
  Polygon result(steps);
  int n = steps - 1;
  double da = (a2 - a1) / n;
  double a = a1;
  for (int i = 0; i <= n; ++i)
  {
    result[i].X = pt.X + Round(std::cos(a)*r);
    result[i].Y = pt.Y + Round(std::sin(a)*r);
    a += da;
  }
  return result;
}
//------------------------------------------------------------------------------

DoublePoint GetUnitNormal( const IntPoint &pt1, const IntPoint &pt2)
{
  double dx = (double)(pt2.X - pt1.X);
  double dy = (double)(pt2.Y - pt1.Y);
  if(  ( dx == 0 ) && ( dy == 0 ) ) return DoublePoint( 0, 0 );

  double f = 1 *1.0/ std::sqrt( dx*dx + dy*dy );
  dx *= f;
  dy *= f;
  return DoublePoint(dy, -dx);
}
//------------------------------------------------------------------------------

bool OffsetPolygons(const Polygons &in_pgs, Polygons &out_pgs, const float &delta)
{

  if (delta == 0)
  {
    if (&in_pgs == &out_pgs) return true;
    out_pgs.assign(in_pgs.begin(), in_pgs.end());
    return true;
  }

  Polygons pgs(in_pgs); //in case in_pgs == out_pgs
  out_pgs.clear();
  out_pgs.reserve(pgs.size());

  double deltaSq = delta*delta;

  for (int j = 0; j < (int)pgs.size(); ++j)
  {
    int highI = (int)pgs[j].size() -1;
    //to minimize artefacts, strip out those polygons where
    //it's shrinking and where its area < Sqr(delta) ...
    double a1 = Area(pgs[j]);
    if (delta < 0) { if (a1 > 0 && a1 < deltaSq) highI = 0;}
    else if (a1 < 0 && -a1 < deltaSq) highI = 0; //nb: a hole if area < 0

    if (highI < 2 && delta <= 0) continue;
    if (highI == 0)
    {
        Polygon arc = BuildArc(pgs[j][highI], 0, pi*2, delta);
        out_pgs.push_back(arc);
        continue;
    }

    Polygon pg;
    pg.reserve(highI*2+2);

    std::vector < DoublePoint > normals(highI+1);
    normals[0] = GetUnitNormal(pgs[j][highI], pgs[j][0]);
    for (int i = 1; i <= highI; ++i)
      normals[i] = GetUnitNormal(pgs[j][i-1], pgs[j][i]);

    for (int i = 0; i < highI; ++i)
    {
      pg.push_back(IntPoint(pgs[j][i].X + Round(delta *normals[i].X),
        pgs[j][i].Y + Round(delta *normals[i].Y)));
      pg.push_back(IntPoint(pgs[j][i].X + Round(delta *normals[i+1].X),
        pgs[j][i].Y + Round(delta *normals[i+1].Y)));
    }
    pg.push_back(IntPoint(pgs[j][highI].X + Round(delta *normals[highI].X),
      pgs[j][highI].Y + Round(delta *normals[highI].Y)));
    pg.push_back(IntPoint(pgs[j][highI].X + Round(delta *normals[0].X),
      pgs[j][highI].Y + Round(delta *normals[0].Y)));

    //round off reflex angles (ie > 180 deg) unless it's almost flat (ie < 10deg angle) ...
    //cross product normals < 0 -> reflex angle; dot product normals == 1 -> no angle
    if ((normals[highI].X *normals[0].Y - normals[0].X *normals[highI].Y) *delta >= 0 &&
    (normals[0].X *normals[highI].X + normals[0].Y *normals[highI].Y) < 0.985)
    {
      double a1 = std::atan2(normals[highI].Y, normals[highI].X);
      double a2 = std::atan2(normals[0].Y, normals[0].X);
      if (delta > 0 && a2 < a1) a2 = a2 + pi*2;
      else if (delta < 0 && a2 > a1) a2 = a2 - pi*2;
      Polygon arc = BuildArc(pgs[j][highI], a1, a2, delta);
      Polygon::iterator it = pg.begin() +highI*2+1;
      pg.insert(it, arc.begin(), arc.end());
    }
    for (int i = highI; i > 0; --i)
      if ((normals[i-1].X*normals[i].Y - normals[i].X*normals[i-1].Y) *delta >= 0 &&
      (normals[i].X*normals[i-1].X + normals[i].Y*normals[i-1].Y) < 0.985)
      {
        double a1 = std::atan2(normals[i-1].Y, normals[i-1].X);
        double a2 = std::atan2(normals[i].Y, normals[i].X);
        if (delta > 0 && a2 < a1) a2 = a2 + pi*2;
        else if (delta < 0 && a2 > a1) a2 = a2 - pi*2;
        Polygon arc = BuildArc(pgs[j][i-1], a1, a2, delta);
        Polygon::iterator it = pg.begin() +(i-1)*2+1;
        pg.insert(it, arc.begin(), arc.end());
      }
    out_pgs.push_back(pg);
  }

  //finally, clean up untidy corners ...
  Clipper c4;
  c4.AddPolygons(out_pgs, ptSubject);
  if (delta > 0){
    return c4.Execute(ctUnion, out_pgs, pftNonZero, pftNonZero);
  }
  else
  {
    IntRect r = c4.GetBounds();
    Polygon outer(4);
    outer[0] = IntPoint(r.left-10, r.bottom+10);
    outer[1] = IntPoint(r.right+10, r.bottom+10);
    outer[2] = IntPoint(r.right+10, r.top-10);
    outer[3] = IntPoint(r.left-10, r.top-10);
    c4.AddPolygon(outer, ptSubject);
    if (c4.Execute(ctUnion, out_pgs, pftNonZero, pftNonZero))
    {
      //erase just the first (outer) polygon
      out_pgs.erase(out_pgs.begin());
      return true;
    }
    else return false;
  }
}
//------------------------------------------------------------------------------

extern "C" {
  //****************** POLYGON
  polygon polygon_new(int numPoints)
  {
    // Polygon * p;
    // if(numPoints > 0)
    //   p = new Polygon(numPoints);
    // else
    //   p = new Polygon();
    // printf("NEW Poly:0x%X\n", p);
    // return p;
    if(numPoints > 0)
      return new Polygon(numPoints);
    else
      return new Polygon();
  }

  void polygon_clear(polygon poly)
  {
    // printf("CLEAR Poly:0x%X\n", poly);
    ((Polygon *) poly)->clear();
  }

  int polygon_size(polygon poly)
  {
    return ((Polygon *) poly)->size();
  }

  void polygon_addPoint(polygon poly, long64 x, long64 y)
  {
    // printf("ADD Point Poly:0x%X, (%lld, %lld)\n", poly, x, y);
    IntPoint pt(x, y);
    ((Polygon *)poly)->push_back(pt);
  }

  long64 polygon_getPointX(polygon poly, int i)
  {
    return ((Polygon *)poly)->at(i).X;
  }

  long64 polygon_getPointY(polygon poly, int i)
  {
    return ((Polygon *)poly)->at(i).Y;
  }

  void polygon_free(polygon poly)
  {
    // printf("FREE Poly:0x%X\n", poly);
    delete (Polygon *) poly;
  }

  int polygon_isClockwise(polygon poly, int useFullInt64Range)
  {
    return IsClockwise(*((Polygon *) poly), (useFullInt64Range == 0)?false:true)?1:0;
  }

  double polygon_getArea(polygon poly, int useFullInt64Range)
  {
    return Area(*((Polygon *) poly), (useFullInt64Range == 0)?false:true);
  } 

  //****************** POLYGONS
  polygons polygons_new(int numPolys)
  {
    // Polygons * ps;
    // if(numPolys > 0)
    //   ps = new Polygons(numPolys);
    // else
    //   ps = new Polygons();
    // printf("NEW Polygons:0x%X\n", ps);
    // return ps;
    if(numPolys > 0)
      return new Polygons(numPolys);
    else
      return new Polygons();
  }

  void polygons_clear(polygons poly)
  {
    // printf("CLEAR Polys: 0x%X\n", poly);
    ((Polygons *) poly)->clear();
  }

  int polygons_size(polygons poly)
  {
    return ((Polygons *) poly)->size();
  }

  polygon polygons_getPoly(polygons polys, int i)
  {
    // Polygon * p = &(((Polygons *)polys)->at(i));
    // printf("GETPoly Polys: 0x%X, Poly:0x%X(%d)\n", polys, p, p->size());
    // return p;
    return &(((Polygons *)polys)->at(i));
  }

  void polygons_addPoly(polygons polys, polygon poly)
  {
    ((Polygons *)polys)->push_back(*((Polygon *)poly));
    // printf("ADDPoly Polys: 0x%X, poly:0x%X(%d), size:%d\n", polys, poly, ((Polygon *)poly)->size(), ((Polygons *)polys)->size());
  }

  void polygons_free(polygons poly)
  {
    // printf("FREE Polys: 0x%X\n", poly);
    delete (Polygons *) poly;
  }


  //****************** EXPOLYGON
  // expolygon expolygon_new(int numPoints)
  // {
  //   if(numPoints > 0)
  //     return new ExPolygons(numPoints);
  //   else
  //     return new ExPolygons();
  // }

  // void expolygon_addPoint(expolygon poly, long64 x, long64 y)
  // {
  //   IntPoint pt(x, y);
  //   ((ExPolygon *)poly)->push_back(pt);
  // }

  // void expolygon_free(expolygon poly)
  // {
  //   delete (ExPolygon *) poly;
  // }

  //****************** CLIPPER
  clipper clipper_new()
  {
    // printf("NEW Clipper\n");
    return new Clipper();
  }

  void clipper_addPolygon(clipper c, polygon poly, PolyType ptype)
  {
    ((Clipper *) c)->AddPolygon(*((Polygon *) poly), ptype);
  }

  void clipper_addPolygons(clipper c, polygons poly, PolyType ptype)
  {
    // printf("AddPolygons CLipper:0x%X, Poly:0x%X, Size:%d\n", c, poly, ((Polygons *) poly)->size());
    ((Clipper *) c)->AddPolygons(*((Polygons *) poly), ptype);
  }

  void clipper_executePoly(clipper c, ClipType ctype, polygons soln)
  {
    // printf("Execute CLipper:0x%X, Soln:0x%X\n", c, soln);
    ((Clipper *) c)->Execute(ctype, *((Polygons *) soln));
  }

  // void clipper_executeExPoly(clipper c, ClipType ctype, polygons soln)
  // {
  //   ((Clipper *) c)->AddPolygons(*((ExPolygons *) poly));
  // }

  void clipper_free(clipper c)
  {
    // printf("FREE Clipper\n");
    delete ((Clipper *) c);
  }
}
