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

#ifndef clipper_hpp
#define clipper_hpp

#include <vector>
#include <stdexcept>
#include <cstring>
#include <cstdlib>
#include <string>

enum ClipType { ctIntersection=0x1, ctUnion=0x2, ctDifference=0x3, ctXor=0x4 };
enum PolyType { ptSubject=0x1, ptClip=0x2 };
enum PolyFillType { pftEvenOdd=0x1, pftNonZero=0x2 };

typedef signed long long long64;
typedef unsigned long long ulong64;

struct IntPoint {
  long64 X;
  long64 Y;
  IntPoint(long64 x = 0, long64 y = 0): X(x), Y(y) {};
};

typedef std::vector< IntPoint > Polygon;
typedef std::vector< Polygon > Polygons;

struct ExPolygon {
  Polygon  outer;
  Polygons holes;
};
typedef std::vector< ExPolygon > ExPolygons;

bool IsClockwise(const Polygon &poly, bool UseFullInt64Range = true);
double Area(const Polygon &poly, bool UseFullInt64Range = true);
bool OffsetPolygons(const Polygons &in_pgs, Polygons &out_pgs, const float &delta);

extern "C" {

  typedef void * polygon;
  typedef void * polygons;
  typedef void * expolygon;
  typedef void * expolygons;
  typedef void * clipper;

  polygon polygon_new(int numPoints);
  void polygon_clear(polygon poly);
  int polygon_size(polygon poly);
  void polygon_addPoint(polygon poly, long64 x, long64 y);
  long64 polygon_getPointX(polygon poly, int i);
  long64 polygon_getPointY(polygon poly, int i);
  void polygon_free(polygon poly);

  int polygon_isClockwise(polygon poly, int useFullInt64Range);
  double polygon_getArea(polygon poly, int useFullInt64Range);

  polygons polygons_new(int numPolys);
  void polygons_clear(polygons poly);
  int polygons_size(polygons poly);
  polygon polygons_getPoly(polygons polys, int i);
  void polygons_addPoly(polygons polys, polygon poly);
  void polygons_free(polygons poly);

  clipper clipper_new();
  void clipper_addPolygon(clipper c, polygon poly, PolyType ptype);
  void clipper_addPolygons(clipper c, polygons poly, PolyType ptype);
  void clipper_executePoly(clipper c, ClipType ctype, polygons soln);
  void clipper_free(clipper c);
}

//used internally ...
enum EdgeSide { esLeft, esRight };
enum IntersectProtects { ipNone = 0, ipLeft = 1, ipRight = 2, ipBoth = 3 };

struct TEdge {
  long64 xbot;
  long64 ybot;
  long64 xcurr;
  long64 ycurr;
  long64 xtop;
  long64 ytop;
  double dx;
  long64 tmpX;
  PolyType polyType;
  EdgeSide side;
  int windDelta; //1 or -1 depending on winding direction
  int windCnt;
  int windCnt2; //winding count of the opposite polytype
  int outIdx;
  TEdge *next;
  TEdge *prev;
  TEdge *nextInLML;
  TEdge *nextInAEL;
  TEdge *prevInAEL;
  TEdge *nextInSEL;
  TEdge *prevInSEL;
};

struct IntersectNode {
  TEdge          *edge1;
  TEdge          *edge2;
  IntPoint        pt;
  IntersectNode  *next;
};

struct LocalMinima {
  long64        Y;
  TEdge        *leftBound;
  TEdge        *rightBound;
  LocalMinima  *next;
};

struct Scanbeam {
  long64    Y;
  Scanbeam *next;
};

struct OutPt; //forward declaration

struct OutRec {
  int     idx;
  bool    isHole;
  OutRec *FirstLeft;
  OutRec *AppendLink;
  OutPt  *pts;
  OutPt  *bottomPt;
};

struct OutPt {
  int     idx;
  IntPoint pt;
  OutPt   *next;
  OutPt   *prev;
};

struct JoinRec {
  IntPoint  pt1a;
  IntPoint  pt1b;
  int       poly1Idx;
  IntPoint  pt2a;
  IntPoint  pt2b;
  int       poly2Idx;
};

struct HorzJoinRec {
  TEdge    *edge;
  int       savedIdx;
};

struct IntRect { long64 left; long64 top; long64 right; long64 bottom; };

typedef std::vector < OutRec* > PolyOutList;
typedef std::vector < TEdge* > EdgeList;
typedef std::vector < JoinRec* > JoinList;
typedef std::vector < HorzJoinRec* > HorzJoinList;

//ClipperBase is the ancestor to the Clipper class. It should not be
//instantiated directly. This class simply abstracts the conversion of sets of
//polygon coordinates into edge objects that are stored in a LocalMinima list.
class ClipperBase
{
public:
  ClipperBase();
  virtual ~ClipperBase();
  bool AddPolygon(const Polygon &pg, PolyType polyType);
  bool AddPolygons( const Polygons &ppg, PolyType polyType);
  virtual void Clear();
  IntRect GetBounds();
  bool UseFullCoordinateRange() {return m_UseFullRange;};
  void UseFullCoordinateRange(bool newVal);
protected:
  void DisposeLocalMinimaList();
  TEdge* AddBoundsToLML(TEdge *e);
  void PopLocalMinima();
  virtual void Reset();
  void InsertLocalMinima(LocalMinima *newLm);
  LocalMinima      *m_CurrentLM;
  LocalMinima      *m_MinimaList;
  bool              m_UseFullRange;
  EdgeList          m_edges;
};

class Clipper : public virtual ClipperBase
{
public:
  Clipper();
  ~Clipper();
  bool Execute(ClipType clipType,
    Polygons &solution,
    PolyFillType subjFillType = pftEvenOdd,
    PolyFillType clipFillType = pftEvenOdd);
  bool Execute(ClipType clipType,
    ExPolygons &solution,
    PolyFillType subjFillType = pftEvenOdd,
    PolyFillType clipFillType = pftEvenOdd);
  void Clear();
protected:
  void Reset();
  virtual bool ExecuteInternal(bool fixHoleLinkages);
private:
  PolyOutList       m_PolyOuts;
  JoinList          m_Joins;
  HorzJoinList      m_HorizJoins;
  ClipType          m_ClipType;
  Scanbeam         *m_Scanbeam;
  TEdge           *m_ActiveEdges;
  TEdge           *m_SortedEdges;
  IntersectNode    *m_IntersectNodes;
  bool              m_ExecuteLocked;
  PolyFillType      m_ClipFillType;
  PolyFillType      m_SubjFillType;
  void DisposeScanbeamList();
  void SetWindingCount(TEdge& edge);
  bool IsNonZeroFillType(const TEdge& edge) const;
  bool IsNonZeroAltFillType(const TEdge& edge) const;
  void InsertScanbeam(const long64 Y);
  long64 PopScanbeam();
  void InsertLocalMinimaIntoAEL(const long64 botY);
  void InsertEdgeIntoAEL(TEdge *edge);
  void AddEdgeToSEL(TEdge *edge);
  void CopyAELToSEL();
  void DeleteFromSEL(TEdge *e);
  void DeleteFromAEL(TEdge *e);
  void UpdateEdgeIntoAEL(TEdge *&e);
  void SwapPositionsInSEL(TEdge *edge1, TEdge *edge2);
  bool IsContributing(const TEdge& edge) const;
  bool IsTopHorz(const long64 XPos);
  void SwapPositionsInAEL(TEdge *edge1, TEdge *edge2);
  void DoMaxima(TEdge *e, long64 topY);
  void ProcessHorizontals();
  void ProcessHorizontal(TEdge *horzEdge);
  void AddLocalMaxPoly(TEdge *e1, TEdge *e2, const IntPoint &pt);
  void AddLocalMinPoly(TEdge *e1, TEdge *e2, const IntPoint &pt);
  void AppendPolygon(TEdge *e1, TEdge *e2);
  void DoEdge1(TEdge *edge1, TEdge *edge2, const IntPoint &pt);
  void DoEdge2(TEdge *edge1, TEdge *edge2, const IntPoint &pt);
  void DoBothEdges(TEdge *edge1, TEdge *edge2, const IntPoint &pt);
  void IntersectEdges(TEdge *e1, TEdge *e2,
    const IntPoint &pt, IntersectProtects protects);
  void AddOutPt(TEdge *e, const IntPoint &pt);
  void DisposeAllPolyPts();
  void DisposeOutRec(int index, bool ignorePts = false);
  bool ProcessIntersections( const long64 topY);
  void AddIntersectNode(TEdge *e1, TEdge *e2, const IntPoint &pt);
  void BuildIntersectList(const long64 topY);
  void ProcessIntersectList();
  void ProcessEdgesAtTopOfScanbeam(const long64 topY);
  void BuildResult(Polygons& polys);
  void BuildResultEx(ExPolygons& polys);
  void SetHoleState(TEdge *e, OutRec *OutRec);
  void DisposeIntersectNodes();
  bool FixupIntersections();
  void FixupOutPolygon(OutRec &outRec);
  bool IsHole(TEdge *e);
  void FixHoleLinkage(OutRec *outRec);
  void AddJoin(TEdge *e1, TEdge *e2, int e1OutIdx = -1, int e2OutIdx = -1);
  void ClearJoins();
  void AddHorzJoin(TEdge *e, int idx);
  void ClearHorzJoins();
  void JoinCommonEdges();
};

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

class clipperException : public std::exception
{
  public:
    clipperException(const char* description)
      throw(): std::exception(), m_description (description) {}
    virtual ~clipperException() throw() {}
    virtual const char* what() const throw() {return m_description.c_str();}
  private:
    std::string m_description;
};
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

#endif //clipper_hpp


