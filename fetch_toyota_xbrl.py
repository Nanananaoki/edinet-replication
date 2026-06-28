#!/usr/bin/env python3
"""
EDINET API v2 - トヨタ自動車の有価証券報告書から当期純利益・発行済株式数を取得するスクリプト
"""

import os
import time
import zipfile
import io
from datetime import date, timedelta

import requests
from lxml import etree

EDINET_API_KEY = os.environ.get("EDINET_API_KEY", "")
BASE_URL = "https://api.edinet-fsa.go.jp/api/v2"

TOYOTA_EDINET_CODE = "E02144"

# 3月期決算なので有報提出は6月頃。直近数年の6月を候補にする
CANDIDATE_DATES = [
    date(2024, 6, 25),
    date(2024, 6, 24),
    date(2024, 6, 26),
    date(2024, 6, 27),
    date(2024, 6, 28),
    date(2023, 6, 26),
    date(2023, 6, 27),
    date(2023, 6, 28),
]


def check_api_key():
    if not EDINET_API_KEY:
        print("[ERROR] 環境変数 EDINET_API_KEY が設定されていません。")
        print("  export EDINET_API_KEY='your_api_key' を実行してください。")
        raise SystemExit(1)


def get_params(**kwargs):
    params = {"Subscription-Key": EDINET_API_KEY}
    params.update(kwargs)
    return params


def find_toyota_doc_id():
    """書類一覧APIを使ってトヨタ自動車の有価証券報告書docIDを探す"""
    print("=== トヨタ自動車の有報docIDを検索中 ===")

    for target_date in CANDIDATE_DATES:
        date_str = target_date.strftime("%Y-%m-%d")
        print(f"\n[検索] 提出日: {date_str}")

        params = get_params(date=date_str, type=2)
        resp = requests.get(f"{BASE_URL}/documents.json", params=params, timeout=30)

        if resp.status_code != 200:
            print(f"  [ERROR] ステータス: {resp.status_code}")
            print(f"  レスポンス: {resp.text[:300]}")
            time.sleep(3)
            continue

        data = resp.json()
        results = data.get("results", []) or []

        for doc in results:
            edinet_code = doc.get("edinetCode", "")
            ordinance = doc.get("ordinanceCode", "")
            form = doc.get("formCode", "")
            filer = doc.get("filerName", "")
            doc_id = doc.get("docID", "")

            if (
                edinet_code == TOYOTA_EDINET_CODE
                and ordinance == "010"
                and form == "030000"
            ):
                print(f"  [発見] filerName={filer}, docID={doc_id}, 提出日={date_str}")
                return doc_id, date_str

        print(f"  該当なし（{len(results)}件中）")
        time.sleep(3)

    print("\n[ERROR] 対象期間内にトヨタ自動車の有報が見つかりませんでした。")
    print("CANDIDATE_DATES を調整して再試行してください。")
    raise SystemExit(1)


def download_xbrl_zip(doc_id):
    """書類取得API（type=1）でXBRL ZIPをダウンロード"""
    print(f"\n=== XBRLダウンロード (docID={doc_id}) ===")

    params = get_params(type=1)
    url = f"{BASE_URL}/documents/{doc_id}"
    resp = requests.get(url, params=params, timeout=120, stream=True)

    if resp.status_code != 200:
        print(f"[ERROR] ステータス: {resp.status_code}")
        print(f"レスポンス: {resp.text[:300]}")
        raise SystemExit(1)

    content = resp.content
    print(f"  ダウンロード完了: {len(content) / 1024:.1f} KB")
    return content


def extract_xbrl_from_zip(zip_bytes):
    """ZIPからXBRLファイルを取り出す"""
    print("\n=== ZIPを解凍してXBRLファイルを探す ===")

    with zipfile.ZipFile(io.BytesIO(zip_bytes)) as zf:
        names = zf.namelist()
        xbrl_files = [n for n in names if n.lower().endswith(".xbrl")]

        print(f"  ZIP内ファイル数: {len(names)}")
        print(f"  XBRLファイル: {xbrl_files}")

        if not xbrl_files:
            print("[ERROR] ZIPにXBRLファイルが見つかりません。")
            print("ZIP内容:", names[:20])
            raise SystemExit(1)

        # 複数ある場合はパスが短い（ルートに近い）ものを優先
        target = sorted(xbrl_files, key=lambda x: len(x))[0]
        print(f"  使用するXBRL: {target}")
        return zf.read(target)


def parse_xbrl(xbrl_bytes):
    """XBRLをパースして当期純利益・発行済株式数を抽出"""
    print("\n=== XBRLをパース ===")

    root = etree.fromstring(xbrl_bytes)

    # 名前空間マップを収集
    ns_map = {}
    for elem in root.iter():
        ns_map.update(elem.nsmap)

    # 全タグ名（ローカル名）を収集
    all_local_names = set()
    for elem in root.iter():
        local = etree.QName(elem.tag).localname if "}" in elem.tag else elem.tag
        all_local_names.add(local)

    # 候補タグのキーワード
    profit_keywords = [
        "NetIncomeLoss",
        "ProfitLossAttributableToOwnersOfParent",
        "ProfitLoss",
        "NetIncome",
        "profit",
        "loss",
        "Income",
    ]
    shares_keywords = [
        "NumberOfIssuedShares",
        "IssuedShares",
        "TotalNumberOfIssuedShares",
        "shares",
        "Shares",
        "株式数",
    ]

    print("\n--- 純利益関連タグ候補 ---")
    profit_candidates = sorted(
        [t for t in all_local_names if any(k.lower() in t.lower() for k in profit_keywords)]
    )
    for t in profit_candidates:
        print(f"  {t}")

    print("\n--- 発行済株式数関連タグ候補 ---")
    shares_candidates = sorted(
        [t for t in all_local_names if any(k.lower() in t.lower() for k in shares_keywords)]
    )
    for t in shares_candidates:
        print(f"  {t}")

    # 値を抽出するヘルパー
    def find_value(keywords, context_hint="Duration"):
        """優先度順にタグを探し、contextRefがDurationまたはInstantのものを返す"""
        for keyword in keywords:
            for elem in root.iter():
                local = etree.QName(elem.tag).localname if "}" in elem.tag else elem.tag
                if keyword.lower() in local.lower():
                    context_ref = elem.get("contextRef", "")
                    text = (elem.text or "").strip()
                    if text and text.lstrip("-").isdigit():
                        return local, text, context_ref
        return None, None, None

    print("\n=== 抽出結果 ===")

    # 当期純利益
    profit_priority = [
        "ProfitLossAttributableToOwnersOfParent",
        "NetIncomeLoss",
        "ProfitLoss",
        "NetIncome",
    ]
    tag, value, ctx = find_value(profit_priority)
    if value:
        val_million = int(value) // 1_000_000 if abs(int(value)) >= 1_000_000 else int(value)
        print(f"\n当期純利益:")
        print(f"  タグ名     : {tag}")
        print(f"  contextRef : {ctx}")
        print(f"  値（原単位）: {int(value):,}")
        # EDINETのXBRLは通常「円」単位または「百万円」単位
        # decimalsで確認
        for elem in root.iter():
            local = etree.QName(elem.tag).localname if "}" in elem.tag else elem.tag
            if tag and tag.lower() in local.lower() and (elem.text or "").strip() == value:
                decimals = elem.get("decimals", "不明")
                unit_ref = elem.get("unitRef", "不明")
                print(f"  decimals   : {decimals}")
                print(f"  unitRef    : {unit_ref}")
                # decimals="-6" → 百万円単位, decimals="0" → 円単位
                if decimals == "-6":
                    print(f"\n当期純利益：{int(value):,} 百万円")
                elif decimals == "0" or decimals == "-3":
                    divisor = 1 if decimals == "0" else 1_000
                    print(f"\n当期純利益：{int(value) // (1_000_000 // divisor):,} 百万円（{int(value):,} 円換算）")
                else:
                    print(f"\n当期純利益：{int(value):,}（単位はdecimalsを確認）")
                break
    else:
        print("\n当期純利益: タグが見つかりませんでした。")
        print("上記の「純利益関連タグ候補」からタグ名を確認してください。")

    # 発行済株式数
    shares_priority = [
        "NumberOfIssuedSharesSummaryOfBusinessResults",
        "TotalNumberOfIssuedShares",
        "NumberOfIssuedShares",
        "IssuedShares",
    ]
    tag_s, value_s, ctx_s = find_value(shares_priority, context_hint="Instant")
    if value_s:
        print(f"\n発行済株式数:")
        print(f"  タグ名     : {tag_s}")
        print(f"  contextRef : {ctx_s}")
        print(f"  値         : {int(value_s):,} 株")
        print(f"\n発行済株式数：{int(value_s):,} 株")
    else:
        print("\n発行済株式数: タグが見つかりませんでした。")
        print("上記の「発行済株式数関連タグ候補」からタグ名を確認してください。")


def main():
    check_api_key()

    doc_id, submit_date = find_toyota_doc_id()
    time.sleep(4)

    zip_bytes = download_xbrl_zip(doc_id)
    time.sleep(3)

    xbrl_bytes = extract_xbrl_from_zip(zip_bytes)

    parse_xbrl(xbrl_bytes)


if __name__ == "__main__":
    main()
